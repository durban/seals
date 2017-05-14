/*
 * Copyright 2016-2017 Daniel Urban
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sigs.seals
package checker

import scala.io.Codec
import scala.collection.JavaConverters._

import scala.reflect.runtime.{ universe => ru }
import scala.reflect.io.AbstractFile

import cats.implicits._

import io.circe.{ Json, Encoder }

import circe.Codecs._

object Extractor {

  final val classExt = ".class"

  def apply(classloader: ClassLoader, jarOrDir: java.io.File): Extractor =
    new Extractor(classloader, jarOrDir)

  def main(args: Array[String]): Unit = {
    val jarOrDir :: target :: packs = args.toList
    val main = apply(this.getClass.getClassLoader, new java.io.File(jarOrDir))
    val res = main.extractAllPackages(packs.toVector)
    java.nio.file.Files.write(
      java.nio.file.Paths.get(target),
      List(res.spaces2).asJava
    )
  }
}

class Extractor(classloader: ClassLoader, jarOrDir: java.io.File) {

  import ru._
  import Extractor._

  val mirror = runtimeMirror(classloader)
  val root = AbstractFile.getDirectory(new scala.reflect.io.File(jarOrDir)(Codec.UTF8))

  val encoder = Encoder[Model]

  def allClasses(pack: String): Vector[String] = {
    val p = findPack(root, pack).getOrElse(throw new IllegalArgumentException(sh"no such package: '${pack}'"))
    p.iterator
      .filter(!_.isDirectory)
      .map(_.name)
      .filter(_.endsWith(classExt))
      .map(_.dropRight(classExt.length))
      .filter(_.nonEmpty)
      .toVector
  }

  def findPack(from: AbstractFile, pack: String): Option[AbstractFile] = {
    val idx = pack.indexWhere(_ === '.')
    if (idx === -1) {
      lookupSubdir(from, pack)
    } else {
      val (h, t) = (pack.take(idx), pack.drop(idx + 1))
      lookupSubdir(from, h).flatMap(f => findPack(f, t))
    }
  }

  def lookupSubdir(from: AbstractFile, subdir: String): Option[AbstractFile] =
    from.iterator.find(f => (f.name === subdir) && f.isDirectory)

  def extractAllPackages(packs: Vector[String]): Json = {
    Json.obj(packs.map { pack =>
      pack -> extractAll(pack)
    }: _*)
  }

  def extractAll(pack: String): Json = {
    val models = allSchemasOfPackage(pack).map { sym =>
      sym.fullName -> extract(sym)
    }
    Json.obj(models: _*)
  }

  def allSchemasOfPackage(pack: String): Vector[Symbol] = {
    val classes = allClasses(pack)
    classes.flatMap { clsName =>
      if (clsName.endsWith("$")) {
        // it's a module
        val modName = clsName.dropRight(1)
        val mods = try {
          Vector(mirror.staticModule(sh"${pack}.${modName}"))
        } catch {
          case _: ScalaReflectionException =>
            // probably a nested module,
            // we'll get it through its parent
            Vector.empty
        }
        mods.flatMap(allSchemasOfModule)
      } else {
        // it's a class
        val clss = try {
          Vector(mirror.staticClass(sh"${pack}.${clsName}"))
        } catch {
          case _: ScalaReflectionException =>
            // probably an anonymous or otherwise,
            // tricky class, we can ignore it
            Vector.empty
        }
        clss.flatMap(allSchemasOfClass)
      }
    }
  }

  def allSchemasOfClass(cls: Symbol): Option[Symbol] = {
    require(cls.isClass && !cls.isModuleClass)
    if (isSchema(cls)) Some(cls)
    else None
  }

  def allSchemasOfModule(module: Symbol): List[Symbol] = {
    require(module.isModule)
    module.typeSignature.members.sorted.flatMap { sym =>
      if (sym.isClass) {
        allSchemasOfClass(sym)
      } else if (sym.isModule) {
        allSchemasOfModule(sym)
      } else {
        Nil
      }
    }
  }

  def isSchema(sym: Symbol): Boolean = {
    require(sym.isClass && !sym.isModuleClass)
    !sym.name.decodedName.toString.contains('$') && sym.annotations.exists { ann: Annotation =>
      ann.tree match {
        case q"new $cls" if cls.symbol == symbolOf[schemaMarker] => true
        case _ => false
      }
    }
  }

  def extract(cls: Symbol): Json = {
    require(cls.isType)
    val compSym = cls.companion
    val reified = if (compSym.isModule) {
      val compObj = mirror.reflectModule(compSym.asModule).instance match {
        case null => // scalastyle:ignore null
          core.impossible(sh"${compSym} is not a companion object")
        case x: Any =>
          x
      }
      val companion = mirror.reflect(compObj)
      val valName = SchemaMacros.synthName(ru)(cls.asType.name, SchemaMacros.reifiedPrefix)
      val field = companion.symbol.info.member(valName)
      if (field.isTerm) {
        val getter = field.asTerm.getter // workaround for strict-unsealed-patmat problem
        val getterSym = getter match {
          case NoSymbol => core.impossible(sh"no getter found for field ${field}")
          case getter: Symbol if getter.isMethod => getter.asMethod
          case _ => core.impossible(sh"getter is not a method: ${getter}")
        }
        companion.reflectMethod(getterSym).apply()
      } else {
        core.impossible(sh"${field} is not a term symbol")
      }
    } else {
      core.impossible(sh"${cls} has no companion object")
    }
    val model = reified match {
      case reified: Reified[_] =>
        reified.model
      case x: Any =>
        core.impossible(sh"expected a Reified instance, got a(n) '${x.getClass.getName}'")
    }
    encoder.apply(model)
  }
}

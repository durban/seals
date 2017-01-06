/*
 * Copyright 2016 Daniel Urban
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
package extractor

import scala.reflect.runtime.{ universe => ru }
import scala.tools.reflect.ToolBox

import cats.implicits._

import io.circe.{ Json, Encoder }

import io.sigs.seals.circe.Codecs._
import scala.reflect.io.AbstractFile
import scala.io.Codec
import scala.collection.JavaConverters._
import java.lang.IllegalArgumentException

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
  val toolbox = ToolBox(mirror).mkToolBox(options = "-Xlog-implicits")
  val root = AbstractFile.getDirectory(new scala.reflect.io.File(jarOrDir)(Codec.UTF8))

  val encoder = Encoder[Model]

  def allClasses(pack: String): Vector[String] = {
    val p = findPack(root, pack).getOrElse(throw new IllegalArgumentException(s"no such package: '${pack}'"))
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
    val sym = mirror.staticPackage(pack)
    val classes = allClasses(pack)
    classes.flatMap { clsName =>
      if (clsName.endsWith("$")) {
        // it's a module
        val modName = clsName.dropRight(1)
        try {
          val modSym = mirror.staticModule(s"${pack}.${modName}")
          allSchemasOfModule(modSym)
        } catch {
          case ScalaReflectionException(_) =>
            // probably a nested module,
            // we'll get it through its parent
            Vector.empty
        }
      } else {
        // it's a class
        val clsSym = mirror.staticClass(s"${pack}.${clsName}")
        allSchemasOfClass(clsSym)
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
        case q"new $cls" if cls.symbol == symbolOf[core.schemaMarker] => true
        case _ => false
      }
    }
  }

  def extract(cls: Symbol): Json = {
    val tree = q"""${cls.companion}.${TermName(core.SchemaMacros.defName)}().model"""
    val model = toolbox.eval(tree.duplicate) match {
      case m: Model => m
      case _ => core.impossible("expected a Model")
    }
    encoder.apply(model)
  }
}

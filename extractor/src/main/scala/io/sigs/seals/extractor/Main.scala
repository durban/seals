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

import io.circe.{ Json, Encoder }

import io.sigs.seals.circe.Codecs._

object Main extends Main(getClass.getClassLoader) {

  def apply(classloader: ClassLoader): Main =
    new Main(classloader)

  def main(args: Array[String]): Unit = {
    val res = extractAllPackages(args.toVector)
    Console.println(res.spaces2)
  }
}

class Main(classloader: ClassLoader) {

  import ru._

  val mirror = runtimeMirror(classloader)
  val toolbox = ToolBox(mirror).mkToolBox(options = "-Xlog-implicits")

  val encoder = Encoder[Model]

  def extractAllPackages(packs: Vector[String]): Json = {
    Json.obj(packs.map { pack =>
      pack -> extractAll(pack)
    }: _*)
  }

  def extractAll(pack: String): Json = {
    val models = allSchemas(pack).map { sym =>
      sym.fullName -> extract(sym)
    }
    Json.obj(models: _*)
  }

  def allSchemas(pack: String): List[Symbol] =
    allSchemas(mirror.staticPackage(pack))

  def allSchemas(packageOrModule: Symbol): List[Symbol] = {
    packageOrModule.typeSignature.members.sorted.flatMap { sym =>
      if (sym.isClass) {
        // can be a schema:
        val cls = mirror.staticClass(sym.fullName)
        if (isSchema(cls)) {
          cls :: Nil
        } else {
          Nil
        }
      } else if (sym.isModule) {
        // can contain schemata:
        val mod = mirror.staticModule(sym.fullName)
        allSchemas(mod)
      } else {
        // no other possibilities:
        Nil
      }
    }
  }

  def isSchema(sym: Symbol): Boolean = {
    sym.isClass && sym.annotations.exists { ann: Annotation =>
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

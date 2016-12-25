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
package core

import scala.language.experimental.macros

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.reflect.macros.whitebox.{ Context => WContext }
import scala.reflect.macros.blackbox.{ Context => BContext }

object SchemaExtractor {

  import scala.reflect.runtime.universe._

  object MethodSymbol {
    def unapply(sym: Symbol): Option[MethodSymbol] = {
      if (sym.isMethod) Some(sym.asMethod)
      else None
    }
  }

  def extract(name: String, field: String): String = {
    val m = runtimeMirror(getClass.getClassLoader)
    m.staticModule(name).typeSignature.decl(TermName(field)) match {
      case MethodSymbol(sym) if sym.isAccessor =>
        sym.returnType match {
          case ct: ConstantTypeApi =>
            ct.value.value match {
              case s: String => s
            }
        }
    }
  }
}

@compileTimeOnly("enable the Macro Paradise compiler plugin to expand macro annotations")
final class schema extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.schemaImpl
}

final class schemaMarker extends StaticAnnotation

object SchemaMacros {

  final val valName = "$io$sigs$seals$core$Reified$Instance"
  final val defName = valName + "$Forwarder"

  def constModel[A]: String = macro constModelImpl[A]

  def constModelImpl[A: c.WeakTypeTag](c: WContext): c.Expr[String] = {
    import c.universe._
    val reiTpe: Type = typeOf[Reified[_]].typeConstructor
    val tpe: Type = weakTypeOf[A]
    val imp: Tree = c.inferImplicitValue(appliedType(reiTpe, List(tpe)), silent = false)
    val tree: Tree = q"_root_.io.sigs.seals.core.Reified[${tpe}](${imp}).model"
    val m: Model = c.eval(c.Expr[Model](c.untypecheck(tree.duplicate)))
    val s: String = m.toString
    c.Expr[String](q"$s")
  }

  def schemaImpl(c: BContext)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    val tree = annottees.map(_.tree) match {

      case List(cls @ ClassDef(mods, name, tparam, impl)) =>

        val newMods = mods.mapAnnotations { anns =>
          anns.filter {
            case pq"_root_.io.sigs.seals.core.schema" => false
            case _ => true
          } :+ q"new _root_.io.sigs.seals.core.schemaMarker()"
        }

        q"""
          ${ClassDef(newMods, name, tparam, impl)}

          object ${name.toTermName} {
            final lazy val ${TermName(valName)}: _root_.io.sigs.seals.core.Reified[${name}] =
              _root_.shapeless.cachedImplicit[_root_.io.sigs.seals.core.Reified[${name}]]
            final def ${TermName(defName)}() =
              this.${TermName(valName)}
          }
        """

      case h :: t =>
        c.abort(c.enclosingPosition, s"Invalid annotation target: ${h} (${h.getClass})")

      case _ =>
        c.abort(c.enclosingPosition, "Invalid annotation target")
    }

    c.Expr[Any](tree)
  }
}

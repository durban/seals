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

import scala.annotation.{ StaticAnnotation, compileTimeOnly }
import scala.reflect.macros.whitebox.{ Context => WContext }
import scala.reflect.macros.blackbox.{ Context => BContext }

private[seals] object SchemaExtractor {

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
              case _ => throw new IllegalArgumentException("not a String")
            }
          case _ =>
            throw new IllegalArgumentException("not a ConstantType")
        }
      case _ =>
        throw new IllegalArgumentException(s"${field} is not a field accessor")
    }
  }
}

/**
 * Macro annotation to designate a type as a schema.
 *
 * When used on a class or trait, it
 *  - marks the class or trait as a schema (with `schemaMarker`);
 *  - and injects a cached implicit into the companion
 *    object which can retrieve a `Reified` instance
 *    for the class or trait.
 */
@compileTimeOnly("enable the Macro Paradise compiler plugin to expand macro annotations")
final class schema extends StaticAnnotation { // scalastyle:ignore class.name
  def macroTransform(annottees: Any*): Any = macro SchemaMacros.schemaImpl
}

/**
 * Don't use this directly.
 *
 * @see `schema` for marking schemata
 */
final class schemaMarker extends StaticAnnotation // scalastyle:ignore class.name

private[seals] object SchemaMacros {

  final val reifiedPrefix = "reified"

  // TODO: make this work or remove
  def constModel[A]: String = macro constModelImpl[A]

  // TODO: make this work or remove
  def constModelImpl[A: c.WeakTypeTag](c: WContext): c.Expr[String] = {
    import c.universe._
    val reiTpe: Type = typeOf[Reified[_]].typeConstructor
    val lzyTpe: Type = typeOf[shapeless.Lazy[_]].typeConstructor
    val tpe: Type = weakTypeOf[A].dealias
    val imp: Tree = c.inferImplicitValue(
      appliedType(lzyTpe, List(appliedType(reiTpe, List(tpe)))),
      silent = false
    )

    val tr: Transformer = new Transformer {
      override def transform(tree: Tree): Tree = {
        super.transform {
          tree match {
            case q"lazy private[this] val $nme: $tpe = $_" =>
              EmptyTree
            case df @ DefDef(mods, nme, tparam, params, ret, body)
                if df.symbol.asTerm.isLazy && df.symbol.asTerm.isAccessor && df.symbol.asTerm.isStable =>
              body match {
                case q"$lhs = $exp; $r" if lhs.symbol == r.symbol =>
                  val rep = q"lazy val $nme = $exp"
                  rep
                case x =>
                  df
              }
            case t =>
              t
          }
        }
      }
    }

    val imp2 = tr.transform(imp)
    val tree: Tree = q"""${imp2}.value.model"""
    val treeToEval = c.untypecheck(tree.duplicate)
    val m: Model = c.eval(c.Expr[Model](treeToEval))
    val s: String = m.toString
    c.Expr[String](q"$s")
  }

  def synthName[U <: scala.reflect.api.Universe](u: U)(tn: u.TypeName, prefix: String): u.TermName =
    u.TermName(prefix + tn.decodedName.toString)

  def schemaImpl(c: BContext)(annottees: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    def transformAnns(anns: List[c.Tree]): List[c.Tree] = {
      anns.filter {
        case pq"_root_.io.sigs.seals.schema" => false
        case _ => true
      } :+ q"new _root_.io.sigs.seals.schemaMarker()"
    }

    def injectedDefis(cls: TypeName): List[c.Tree] = {
      val valName = synthName(c.universe)(cls, reifiedPrefix)
      List(
        q"""
          implicit final lazy val ${valName}: _root_.io.sigs.seals.core.Reified[${cls}] =
            _root_.shapeless.cachedImplicit[_root_.io.sigs.seals.core.Reified[${cls}]]
        """
      )
    }

    val tree = annottees.map(_.tree) match {

      case List(ClassDef(mods, name, tparam, impl)) =>
        val newMods = mods.mapAnnotations(transformAnns(_))
        q"""
          ${ClassDef(newMods, name, tparam, impl)}

          object ${name.toTermName} {
            ..${injectedDefis(name)}
          }
        """

      case List(ClassDef(cMods, cName, tparam, cImpl), ModuleDef(oMods, oName, Template(ps, sl, body))) =>
        val newCMods = cMods.mapAnnotations(transformAnns(_))
        val newBody = body ++ injectedDefis(cName)
        q"""
          ${ClassDef(newCMods, cName, tparam, cImpl)}
          ${ModuleDef(oMods, oName, Template(ps, sl, newBody))}
        """

      case h :: t =>
        c.abort(c.enclosingPosition, s"Invalid annotation target: ${h} (${h.getClass})")

      case _ =>
        c.abort(c.enclosingPosition, "Invalid annotation target")
    }

    c.Expr[Any](tree)
  }
}

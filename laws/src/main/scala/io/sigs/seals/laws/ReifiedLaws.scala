/*
 * Copyright 2016-2017 Daniel Urban and contributors listed in AUTHORS
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
package laws

import scala.annotation.tailrec

import cats.{ Eq, Show }
import cats.implicits._
import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

object ReifiedLaws {

  def apply[A](implicit a: Arbitrary[A], r: Reified[A], e: Eq[A]): ReifiedLaws[A] = new ReifiedLaws[A] {
    def Arb = a
    def Rei = r
    def Equ = e
  }

  sealed trait Tree {

    /**
     * A simplified Tree, which doesn't
     * care about product nesting and
     * labels. (Useful for testing the
     * invariant monoidal functor laws).
     */
    final def simplified: Tree =
      fixpoint(this)(_.simplify)

    private[this] def fixpoint[A: Eq](z: A)(f: A => A): A = {
      @tailrec
      def go(last: A): A = {
        val curr = f(last)
        if (curr === last) curr
        else go(curr)
      }
      go(z)
    }

    protected final def simplify: Tree = this match {
      case PNil => this
      case PCons(_, h, t) => t match {
        case PNil => h.simplify
        case t @ PCons(_, _, _) => h.simplify match {
          case PNil =>
            t.simplify
          case PCons(_, hh, ht) =>
            hh.simplify ++ ht.simplify ++ t.simplify
          case h: Tree =>
            h.simplify ++ t.simplify
        }
      }
      case Sum(_, _) => this
      case Atom(_) => this
      case Vect(els) => Vect(els.map(_.simplify))
    }

    protected final def ++ (that: Tree): Tree = this match {
      case PNil =>
        that
      case PCons(_, h, t) => (t.simplify ++ that.simplify).simplify match {
        case tth: Prod => PCons(Tree.emptySym, h.simplify, tth)
        case tth: Tree => PCons(Tree.emptySym, h.simplify, PCons(Tree.emptySym, tth, PNil))
      }
      case Sum(_, _) | Atom(_) | Vect(_) => that.simplify match {
        case that: Prod => PCons(Tree.emptySym, this, that)
        case that: Tree => PCons(Tree.emptySym, this, PCons(Tree.emptySym, that, PNil))
      }
    }
  }

  object Tree {

    final val emptySym = Symbol("'")

    implicit val eqForTree: Eq[Tree] =
      Eq.fromUniversalEquals

    implicit val showForTree: Show[Tree] =
      Show.fromToString[Tree]
  }

  final case class Atom(s: String) extends Tree
  final case class Sum(sym: Symbol, t: Tree) extends Tree
  final case class Vect(els: Vector[Tree]) extends Tree

  sealed trait Prod extends Tree
  final case object PNil extends Prod
  final case class PCons(sym: Symbol, h: Tree, t: Prod) extends Prod

  def foldToTree[A](r: Reified[A], a: A): Tree = {
    r.foldClose(a)(Reified.Folder.simple[Tree](
      atom = a => Atom(a.stringRepr),
      hNil = () => PNil,
      hCons = (s, h, t) => t match {
        case t: Prod => PCons(s, h, t)
        case _ => core.impossible("tail is not a prod")
      },
      sum = Sum.apply,
      vector = Vect.apply
    ))
  }
}

trait ReifiedLaws[A] extends Laws {

  import ReifiedLaws._

  implicit def Arb: Arbitrary[A]
  implicit def Rei: Reified[A]
  implicit def Equ: Eq[A]

  def reified: this.RuleSet = new ReifiedRuleSet(
    name = "reified",
    "fold-unfold" -> forAll { (a: A) =>
      val tree = foldToTree(Rei, a)

      val x: Either[String, (A, Tree)] = Rei.unfold(
        Reified.Unfolder.instance[Tree, String, Vector[Tree]](
          atom = {
            case t @ Atom(s) => Either.right(Reified.StringResult(s, t))
            case _ => Either.left("not atom")
          },
          atomErr = (t, err) => sh"cannot parse ${t}: '${err.msg}'",
          hNil = {
            case PNil => Either.right(PNil)
            case x: Any => Either.left(sh"not HNil: $x")
          },
          hCons = (t, sym) => t match {
            case PCons(`sym`, h, t) => Either.right(Either.right((h, _ => Either.right(t))))
            case _ => Either.left("boo")
          },
          cNil = _ => "CNil",
          cCons = (t, sym) => t match {
            case Sum(`sym`, t2) => Either.right(Left(t2))
            case Sum(_, _) => Either.right(Right(t))
            case _ => Either.left(sh"not CCons: $t")
          },
          vectorInit = t => t match {
            case Vect(els) => Either.right((t, els))
            case _ => Either.left(sh"not Vect: $t")
          },
          vectorFold = (t, v) => if (v.isEmpty) {
            Either.right(None)
          } else {
            Either.right(Some((v.head, v.tail)))
          },
          unknownError = identity
        )
      )(tree)

      x.fold(
        err => Prop.falsified :| sh"error during unfold: ${err}",
        { case (a2, _) => Prop(Equ.eqv(a2, a)) }
      )
    }
  )

  final class ReifiedRuleSet(
    val name: String,
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val parent = None
    val bases = Nil
  }
}

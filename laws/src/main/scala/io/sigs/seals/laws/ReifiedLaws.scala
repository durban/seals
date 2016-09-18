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
package laws

import cats.Eq
import cats.data.Xor
import cats.kernel.laws._
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

  sealed trait Tree
  object Tree {
    implicit val eqForTree: Eq[Tree] =
      Eq.fromUniversalEquals
  }

  final case class Atom(s: String) extends Tree
  final case object PNil extends Tree
  final case class PCons(sym: Symbol, h: Tree, t: Tree) extends Tree
  final case class Sum(sym: Symbol, t: Tree) extends Tree

  def foldToTree[A](r: Reified[A], a: A): Tree = {
    r.fold[Tree](a)(
      atom = Atom.apply,
      hNil = () => PNil,
      hCons = PCons.apply,
      sum = Sum.apply
    )
  }
}

trait ReifiedLaws[A] extends Laws {

  import ReifiedLaws._

  implicit def Arb: Arbitrary[A]
  implicit def Rei: Reified[A]
  implicit def Equ: Eq[A]

  def reified = new ReifiedRuleSet(
    name = "reified",
    "fold-unfold" -> forAll { (a: A) =>
      val tree = foldToTree(Rei, a)
      val x: Xor[String, A] = Rei.unfold[Tree, String](
        atom = {
          case Atom(s) => Xor.right(s)
          case _ => Xor.left("not atom")
        },
        atomErr = t => s"cannot parse $t",
        hNil = {
          case PNil => Xor.right(())
          case x: Any => Xor.left(s"not HNil: $x")
        },
        hCons = (t, sym) => t match {
          case PCons(`sym`, h, t) => Xor.right(Xor.right((h, t)))
          case _ => Xor.left("boo")
        },
        cNil = _ => "CNil",
        cCons = (t, sym) => t match {
          case Sum(`sym`, t2) => Xor.right(Left(t2))
          case Sum(_, _) => Xor.right(Right(t))
          case x: Any => Xor.left(s"not CCons: $x")
        }
      )(tree)

      x.fold(
        err => Prop.falsified,
        a2 => (a2 ?== a)
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

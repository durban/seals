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

import cats.kernel.laws._
import cats.kernel.instances.boolean._
import cats.kernel.Eq

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

object AnyLaws {

  def apply[A](implicit arb: Arbitrary[A]): AnyLaws[A] = new AnyLaws[A] {
    def Arb = arb
  }

  final class Dummy()
}

trait AnyLaws[A] extends Laws {

  import AnyLaws._

  implicit def Arb: Arbitrary[A]

  def equalsHashCode: this.RuleSet = new AnyRuleSet(
    name = "equals-hashCode",
    parent = None,
    bases = List(),
    "equals-hashCode-consistent" -> forAll { (x: A, y: A) =>
      !(x == y) ?|| (x.## == y.##)
    },
    "equals-false-for-other-types" -> forAll { (x: A) =>
      val ok = (x != new Dummy)
      Prop(Result(status = if (ok) True else False))
    }
  )

  def serializability: this.RuleSet = new AnyRuleSet(
    name = "serializability",
    parent = Some(this.equalsHashCode),
    bases = List(),
    Rules.serializable[A]
  )

  def equality(implicit Equ: Eq[A]): this.RuleSet = new AnyRuleSet(
    name = "equality",
    parent = Some(this.serializability),
    bases = List(),
    "equals-Eq-consistent" -> forAll { (x: A, y: A) =>
      Equ.eqv(x, y) ?== (x == y)
    }
  )

  def any(implicit Equ: Eq[A]): this.RuleSet = new AnyRuleSet(
    name = "any",
    parent = Some(this.equality),
    bases = List()
  )

  def referenceEquality: this.RuleSet = new AnyRuleSet(
    name = "referenceEquality",
    parent = None,
    bases = List(),
    "reference-equals" -> forAll { (x: A, y: A) =>
      (x == y) ?== (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
    },
    "identity-hashCode" -> forAll { (x: A, y: A) =>
      // we ignore collisions here, as
      // they should be sufficiently rare
      (x.## == y.##) ?== (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef])
    }
  )

  def equalitySerializability(implicit Equ: Eq[A]): this.RuleSet = new AnyRuleSet(
    name = "equalitySerializability",
    parent = None,
    bases = List(),
    Rules.equalitySerializable[A]
  )

  final class AnyRuleSet(
    val name: String,
    val parent: Option[this.RuleSet],
    val bases: List[(String, Laws#RuleSet)],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent
}

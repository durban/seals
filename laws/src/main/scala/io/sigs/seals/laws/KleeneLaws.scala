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

import cats.Eq
import cats.kernel.laws._
import cats.kernel.laws.discipline._
import cats.instances.all._
import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

object KleeneLaws {
  def apply[F[_], A](
    implicit
    arbA: Arbitrary[A],
    arbFA: Arbitrary[F[A]],
    arbVect: Arbitrary[Vector[A]],
    kle: Kleene[F],
    equA: Eq[A],
    equFA: Eq[F[A]]
  ): KleeneLaws[F, A] = new KleeneLaws[F, A] {
    def ArbA = arbA
    def ArbFA = arbFA
    def ArbVect = arbVect
    def Kle = kle
    def EquA = equA
    def EquFA = equFA
  }
}

trait KleeneLaws[F[_], A] extends Laws {

  implicit def Kle: Kleene[F]
  implicit def ArbA: Arbitrary[A]
  implicit def ArbFA: Arbitrary[F[A]]
  implicit def ArbVect: Arbitrary[Vector[A]]
  implicit def EquA: Eq[A]
  implicit def EquFA: Eq[F[A]]

  def roundtrip: this.RuleSet = new KleeneRuleSet(
    name = "roundtrip",
    "toVector-fromVector" -> forAll { (fa: F[A]) =>
      Kle.fromVector(Kle.toVector(fa)) <-> fa
    },
    "fromVector-toVector" -> forAll { (va: Vector[A]) =>
      Kle.toVector(Kle.fromVector(va)) <-> va
    }
  )

  final class KleeneRuleSet(
    val name: String,
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val parent = None
    val bases = Nil
  }
}

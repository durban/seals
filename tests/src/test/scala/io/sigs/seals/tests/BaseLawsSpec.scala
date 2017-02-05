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
package tests

import java.util.UUID

import cats.Eq
import cats.kernel.laws._

import org.scalatest.FunSuite
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.util.Buildable
import org.typelevel.discipline.scalatest.Discipline

import laws.{ AnyLaws, AtomicLaws }

trait BaseLawsSpec
  extends FunSuite
  with Discipline
  with laws.TestEqInstances
  with laws.TestArbInstances {

  implicit val eqForUuid: Eq[UUID] =
    Eq.fromUniversalEquals

  implicit def eqForJavaEnums[A <: java.lang.Enum[A]]: Eq[A] =
    referenceEq[A]

  def checkAtomicLaws[A](name: String)(implicit a: Arbitrary[A], e: Eq[A], at: Atomic[A]): Unit = {
    checkAll(s"Atomic[$name].AnyLaws.any", AnyLaws[Atomic[A]].any)
    checkAll(s"Atomic[$name].AnyLaws.equalitySerializability", AnyLaws[Atomic[A]].equalitySerializability)
    checkAll(s"Atomic[$name].AnyLaws.referenceEquality", AnyLaws[Atomic[A]].referenceEquality)
    checkAll(s"Atomic[$name].OrderLaws.eqv", OrderLaws[Atomic[A]].eqv)
    checkAll(s"Atomic[$name].AtomicLaws.roundtrip", AtomicLaws[A].roundtrip)
  }

  /**
   * Some generators fail to produce
   * instances quite frequently. When
   * generating big containers of these,
   * Scalacheck tends to give up after
   * a while. For these cases we provide
   * `Arbitrary` instances for small
   * containers. This is the max size of
   * these.
   *
   * @see `LimitedContainers`
   */
  protected val maxContainerSize = 3

  object LimitedContainers {
    implicit def arbCont[F[_], A](
      implicit
      A: Arbitrary[A],
      B: Buildable[A, F[A]],
      T: F[A] => Traversable[A]
    ): Arbitrary[F[A]] = Arbitrary {
      for {
        n <- Gen.choose(0, maxContainerSize)
        v <- Gen.containerOfN[F, A](n, A.arbitrary)
      } yield v
    }
  }
}

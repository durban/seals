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
import cats.implicits._

import shapeless.Nat
import shapeless.ops.nat.ToInt

import org.scalacheck.Arbitrary

/**
 * `cats.Eq` instances which we only need for tests.
 */
trait TestEqInstances {

  def referenceEq[A <: AnyRef]: Eq[A] =
    Eq.instance(_ eq _)

  def testEqForReified[A](implicit arb: Arbitrary[A]): Eq[Reified[A]] = {
    Eq.instance { (x, y) =>
      testInstances(arb).forall { a =>
        val tx = ReifiedLaws.foldToTree(x, a).simplified
        val ty = ReifiedLaws.foldToTree(y, a).simplified
        tx === ty
      }
    }
  }

  protected def sampleSize: Int = 100

  private def testInstances[A](arb: Arbitrary[A]): Stream[A] =
    Stream.continually(arb.arbitrary.sample).flatten.take(sampleSize)

  /** Just forwarding to cats */
  implicit val symbolEq: Eq[Symbol] =
    cats.kernel.instances.symbol.catsKernelStdOrderForSymbol

  implicit val mathContextEq: Eq[java.math.MathContext] =
    Eq.fromUniversalEquals

  implicit def shapelessNatEq[N <: Nat](implicit toInt: ToInt[N]): Eq[N] =
    Eq.by(_ => toInt())
}

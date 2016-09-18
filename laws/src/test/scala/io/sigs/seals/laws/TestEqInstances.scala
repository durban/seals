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
import cats.implicits._

import org.scalacheck.Arbitrary

/**
 * Test `cats.Eq` instances for types
 * which otherwise doesn't have a proper
 * `Eq` instance.
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
}

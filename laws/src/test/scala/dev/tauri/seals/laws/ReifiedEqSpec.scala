/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
 * Copyright 2020 Nokia
 * SPDX-License-Identifier: Apache-2.0
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

package dev.tauri.seals
package laws

import cats.Eq

import shapeless._
import shapeless.syntax.singleton._
import shapeless.record._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Arbitrary

class ReifiedEqSpec extends AnyFlatSpec with Matchers {

  def reifiedEq[A: Arbitrary]: Eq[Reified[A]] =
    ReifiedEqSpec.Helper.testEqForReified

  type XY = Record.`_root_.scala.Symbol("x") -> Int, _root_.scala.Symbol("y") -> String`.T
  type XYZ1 = Record.`_root_.scala.Symbol("a") -> XY, _root_.scala.Symbol("z") -> Float`.T
  type YZ = Record.`_root_.scala.Symbol("y") -> String, _root_.scala.Symbol("z") -> Float`.T
  type XYZ2 = Record.`_root_.scala.Symbol("x") -> Int, _root_.scala.Symbol("a") -> YZ`.T
  implicitly[XYZ1 =:!= XYZ2]

  "The test Eq[Reified] instance" should "allow tuple nesting differences" in {
    implicit def arbXyz1(implicit arbTup: Arbitrary[(Int, String, Float)]): Arbitrary[XYZ1] = Arbitrary {
      for {
        t <- arbTup.arbitrary
      } yield {
        Symbol("a") ->> (Symbol("x") ->> t._1 :: Symbol("y") ->> t._2 :: HNil) ::
        Symbol("z") ->> t._3 ::
        HNil
      }
    }

    val r1: Reified[XYZ1] = Reified[XYZ1]
    val r2: Reified[XYZ1] = Reified[XYZ2].imap[XYZ1] { xyz2 =>
      Symbol("a") ->> (Symbol("x") ->> xyz2.head :: Symbol("y") ->> xyz2.tail.head(Symbol("y")) :: HNil) ::
      Symbol("z") ->> xyz2.tail.head(Symbol("z")) ::
      HNil
    } { xyz1 =>
      Symbol("x") ->> xyz1.head(Symbol("x")) ::
      Symbol("a") ->> (Symbol("y") ->> xyz1.head(Symbol("y")) :: Symbol("z") ->> xyz1.tail.head :: HNil) ::
      HNil
    }

    reifiedEq[XYZ1].eqv(r1, r2) should be (true)
  }

  it should "not allow additional fields" in {
    val r1 = Reified[(Int, String)]
    val r2 = Reified[(Int, String, Float)].imap[(Int, String)] {
      case (i, s, _) => (i, s)
    } {
      case (i, s) => (i, s, 0.0f)
    }

    reifiedEq[(Int, String)].eqv(r1, r2) should be (false)
  }

  it should "not allow field reordering" in {
    val r1 = Reified[(Int, String)]
    val r2 = Reified[(String, Int)].imap[(Int, String)] {
      case (s, i) => (i, s)
    } {
      case (i, s) => (s, i)
    }

    reifiedEq[(Int, String)].eqv(r1, r2) should be (false)
  }
}

object ReifiedEqSpec {
  object Helper extends TestEqInstances
}

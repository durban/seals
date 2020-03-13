/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package refined

import cats.implicits._

import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

import laws.CanonicalRepr
import tests.BaseSpec

class RefinedSpec extends BaseSpec {

  "Reified instances for refinement types" - {

    "Positive" in {
      val k: Int Refined Positive = 89
      val f = CanonicalRepr.fold[Int Refined Positive](k)
      f should === (CanonicalRepr.Atom("89"))
      CanonicalRepr.unfold[Int Refined Positive](f) should === (Right(k))
      CanonicalRepr.unfold[Int Refined Positive](CanonicalRepr.Atom("0")) match {
        case Left(_) => // OK
        case Right(l) => fail(s"unexpected result: ${l}")
      }
    }

    "Greater[0] is the same as Positive" in {
      val r1 = Reified[Int Refined Positive]
      val r2 = Reified[Int Refined Greater[shapeless.Nat._0]]
      val r3 = Reified[Int Refined Greater[shapeless.Nat._1]]

      r1.model should === (r2.model)
      r1.model should !== (r3.model)
    }

    "Greater" in {
      type GE6 = Int Refined Greater[shapeless.Nat._5]
      val f = CanonicalRepr.fold[GE6](6)
      f should === (CanonicalRepr.Atom("6"))
      CanonicalRepr.unfold[GE6](f) should === (Right(6 : GE6))
      CanonicalRepr.unfold[GE6](CanonicalRepr.Atom("5")) match {
        case Left(_) => // OK
        case Right(l) => fail(s"unexpected result: ${l}")
      }
    }

    "Less" in {
      type LT5 = Int Refined Less[shapeless.Nat._5]
      type LT6 = Int Refined Less[shapeless.Nat._6]
      type LT6Float = Float Refined Less[shapeless.Nat._6]
      val r = Reified[LT6]
      assert(!r.model.compatible(Reified[LT5].model))
      assert(!r.model.compatible(Reified[LT6Float].model))
    }
  }
}

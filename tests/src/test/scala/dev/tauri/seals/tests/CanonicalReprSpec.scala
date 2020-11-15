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
package tests

import cats.Order

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import core.CanonicalRepr

import laws.TestArbInstances.{ arbModel, arbCanonicalRepr, arbSymbol }
import laws.TestArbInstances.forTestData.arbDefsAdt1
import laws.TestTypes.adts.defs.Adt1

class CanonicalReprSpec
    extends AnyFlatSpec
    with Matchers
    with ScalaCheckDrivenPropertyChecks {

  val ord = Order[CanonicalRepr]

  "fold-unfold" should "be an identity" in {
    forAll { x: Int => foldUnfold(x) should === (x) }
    forAll { x: String => foldUnfold(x) should === (x) }
    forAll { x: Model => foldUnfold(x) should === (x) }
    forAll { x: Adt1 => foldUnfold(x) should === (x) }
  }

  def foldUnfold[A](a: A)(implicit r: Reified[A]): A =
    CanonicalRepr.roundtrip(a)(r)

  "Order[CanonicalRepr]" should "be consistent with .equals" in {
    forAll { (x: CanonicalRepr, y: CanonicalRepr) =>
      if (ord.eqv(x, y)) {
        x should === (y)
      } else {
        x should !== (y)
      }
    }
  }

  "product" should "create correct HCons/HNil" in {
    import CanonicalRepr.{ HCons, HNil }
    val act = CanonicalRepr.product(
      Symbol("a") -> CanonicalRepr.Atom("1"),
      Symbol("b") -> CanonicalRepr.Atom("2"),
      Symbol("c") -> CanonicalRepr.Atom("3")
    )
    val exp = HCons(
      Symbol("a"),
      CanonicalRepr.Atom("1"),
      HCons(
        Symbol("b"),
        CanonicalRepr.Atom("2"),
        HCons(
          Symbol("c"),
          CanonicalRepr.Atom("3"),
          HNil
        )
      )
    )
    act should === (exp)
    CanonicalRepr.product() should === (HNil)
  }
}

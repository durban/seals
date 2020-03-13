/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.Eq
import cats.implicits._

import laws.TestTypes.adts.defs.Adt1

class EnvelopeSpec extends BaseSpec {

  val a1 = Envelope[Int](42)
  val a2 = Envelope[Int](42)
  val b1 = Envelope[Adt1](Adt1.C(42))
  val b2 = Envelope[Adt1](Adt1.C(42, "boo"))

  "Correct equals/hashCode" in {
    checkEqHash(a1, a2)
    checkEqHash(a2, a1)
    checkEqHash(b1, b2)
    checkEqHash(b2, b1)
    checkNotEqHash(a1, b1)
    checkNotEqHash(b2, a2)
  }

  "cats.Eq" in {
    Eq[Envelope[Int]].eqv(a1, a2) should === (true)
    Eq[Envelope[Int]].eqv(a1, Envelope[Int](67)) should === (false)
    Eq[Envelope[Adt1]].eqv(b1, b2) should === (true)
    Eq[Envelope[Adt1]].eqv(b1, Envelope[Adt1](Adt1.C(76))) should === (false)
  }
}

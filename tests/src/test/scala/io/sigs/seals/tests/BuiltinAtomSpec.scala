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
package tests

class BuiltinAtomSpec extends BaseSpec {

  "Built-in atoms" in {
    val atoms = Vector(
      // primitives:
      Atom[Byte],
      Atom[Short],
      Atom[Char],
      Atom[Int],
      Atom[Long],
      Atom[Float],
      Atom[Double],
      Atom[Boolean],
      Atom[Unit],
      // std types:
      Atom[String],
      Atom[Symbol],
      Atom[BigInt],
      Atom[BigDecimal]
    )
    atoms.map(_.uuid).toSet should have size atoms.size.toLong
    atoms.foreach { a =>
      AtomRegistry.builtinAtomRegistry.getAtom(a.uuid).fold(
        err => fail(err),
        b => b shouldBe theSameInstanceAs (a)
      )
    }
  }
}

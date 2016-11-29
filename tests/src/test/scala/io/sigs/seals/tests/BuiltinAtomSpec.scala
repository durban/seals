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

import java.util.UUID

class BuiltinAtomSpec extends BaseSpec {

  import Model.Atom.atom

  "Built-in atoms" in {
    val atoms = Vector(
      // primitives:
      atom[Byte],
      atom[Short],
      atom[Char],
      atom[Int],
      atom[Long],
      atom[Float],
      atom[Double],
      atom[Boolean],
      atom[Unit],
      // std types:
      atom[String],
      atom[Symbol],
      atom[BigInt],
      atom[BigDecimal],
      atom[UUID]
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

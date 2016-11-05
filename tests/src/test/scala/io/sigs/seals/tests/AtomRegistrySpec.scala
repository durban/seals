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

class AtomRegistrySpec extends BaseSpec {

  "Built-in registry should be in implicit scope" in {
    implicitly[AtomRegistry] shouldBe theSameInstanceAs (
      AtomRegistry.builtinAtomRegistry
    )
  }

  "AtomRegistry" - {

    "equals + hashCode" in {
      val u1 = UUID.randomUUID()
      val u2 = UUID.randomUUID()
      val ar1 = AtomRegistry.fromMap(Map(u1 -> Atom[Int], u2 -> Atom[String]))
      val ar2 = new AtomRegistry {
        override def map = Map(u1 -> Atom[Int], u2 -> Atom[String])
      }
      checkEqHash(ar1, ar2)

      val ar1b = ar1 + Atom[Short]
      val ar2b = ar2 + Atom[Short]
      checkEqHash(ar1b, ar2b)
    }
  }
}

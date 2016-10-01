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

import org.scalatest.prop.GeneratorDrivenPropertyChecks

import shapeless.test.illTyped

import laws.MyUUID
import laws.TestArbInstances.arbUuid
import laws.TestInstances.atomic._
import laws.TestInstances.atomic.bad._
import laws.TestTypes.Whatever

class AtomicSpec extends BaseSpec with GeneratorDrivenPropertyChecks {

  val atom = Atom[MyUUID]
  val whatever = Atom[Whatever.type]

  "Automatic Model derivation" in {
    atom.desc should === ("MyUUID")
    atom.uuid should === (UUID.fromString("85a168db-6ce3-47e7-b8aa-e45aa075d523"))
  }

  "equals + hashCode" in {
    checkEqHash(atom, atom)
    checkNotEqHash(atom, whatever)
    checkNotEqHash(atom, Atom[String])
  }

  "Serializable" in {
    checkSer(atom)
    checkSer(whatever)
  }

  "stringRepr/fromString" in {
    forAll { u: MyUUID =>
      atom.fromString(atom.stringRepr(u)) should === (Some(u))
    }
  }

  "conflicts with built-in Atoms are disallowed" in {
    illTyped(
      "Atom[Int]",
      ".*ambiguous implicit values.*"
    )
  }
}

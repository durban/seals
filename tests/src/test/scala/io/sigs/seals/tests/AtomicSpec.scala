/*
 * Copyright 2016-2017 Daniel Urban
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
import java.math.RoundingMode

import org.scalatest.Inside
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import cats.implicits._

import laws.MyUUID
import laws.TestArbInstances.arbUuid
import laws.TestInstances.atomic._
import laws.TestTypes.{ CaseClass, Whatever }

class AtomicSpec extends BaseSpec with GeneratorDrivenPropertyChecks with Inside {

  val atomic = Atomic[MyUUID]
  val whatever = Atomic[Whatever.type]

  "Automatic Model derivation" in {
    atomic.description should === ("MyUUID")
    atomic.uuid should === (UUID.fromString("85a168db-6ce3-47e7-b8aa-e45aa075d523"))
  }

  "equals + hashCode" in {
    checkEqHash(atomic, atomic)
    checkNotEqHash(atomic, whatever)
    checkNotEqHash(atomic, Atomic[String])
  }

  "Serializable" in {
    checkSer(atomic)
    checkSer(whatever)
  }

  "stringRepr/fromString and binaryRepr/fromBinary" in {
    forAll { u: MyUUID =>
      roundtripStr(u)(atomic) should === (u)
      roundtripBin(u)(atomic) should === (u)
    }
  }

  "custom Atomic has higher priority than built-in" in {
    import laws.TestInstances.atomic.bad._
    Model.Atom.atom[Int].uuid should === (laws.TestInstances.atomic.bad.atomicInt.uuid)
  }

  "have higher priority than generic Reified" in {
    // derived instance:
    val r1 = Reified[CaseClass]

    // define an atomic:
    val constUuid = UUID.fromString("3ba1f17e-c0cd-4b17-8cca-771e90b60498")
    val r2 = {
      object ACC extends Atomic[CaseClass] with Atomic.FallbackBinary[CaseClass] {

        def description: String = "CaseClass"

        def fromString(s: String): Either[Atomic.Error, CaseClass] = {
          try {
            Either.right(CaseClass(s.toLong))
          } catch {
            case ex: IllegalArgumentException => Either.left(Atomic.Error(ex.getMessage))
          }
        }

        def stringRepr(a: CaseClass): String = a.n.toString

        val uuid: UUID = constUuid
      }

      implicit val atomicCaseClass: Atomic[CaseClass] = ACC

      // now this should take priority over the derived instance:
      Reified[CaseClass]
    }

    r1.model should !== (r2.model)

    inside (r1.model) {
      case _: core.Model.Atom =>
        fail("not expected an Atom")
      case _ =>
        // OK
    }

    inside (r2.model) {
      case a: core.Model.Atom =>
        a.uuid should === (constUuid)
    }
  }

  "Atomic.ForEnum have higher priority than Reified#reifiedForEnumLike" in {
    val inst = Atomic[RoundingMode]
    val fromReified = Reified.reifiedForEnumLike[RoundingMode]
    inst shouldBe theSameInstanceAs (Atomic.builtinRoundingMode)
    inst should not be theSameInstanceAs (fromReified)
  }
}

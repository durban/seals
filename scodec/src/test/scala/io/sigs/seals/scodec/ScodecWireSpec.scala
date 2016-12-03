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
package scodec

import cats.~>
import cats.implicits._

import _root_.scodec.bits.BitVector
import _root_.scodec.Err

import io.sigs.seals.laws.ArbInstances
import io.sigs.seals.laws.TestArbInstances
import io.sigs.seals.laws.TestTypes
import io.sigs.seals.laws.WireLaws
import io.sigs.seals.core.Wire

class ScodecWireSpec
    extends tests.BaseLawsSpec
    with ArbInstances {

  import TestArbInstances.forTestData._
  import TestTypes.adts.defs.{ Adt1, Adt2 }
  import TestTypes.adts.recursive.IntList
  import TestTypes.adts.recursive.v2.{ IntList => IntListV2 }
  import TestTypes.collections.{ Adt, Cyclic }

  val mkWire = λ[Reified ~> Wire.Aux[?, BitVector, Err]](
    x => Wires.wireFromReified(x)
  )

  checkAll(
    "WireLaws[Int, BitVector, Err]",
    WireLaws[Int, BitVector, Err](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[String, BitVector, Err]",
    WireLaws[String, BitVector, Err](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[Vector[String], BitVector, Err]",
    WireLaws[Vector[String], BitVector, Err](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[List[IntList], BitVector, Err]",
    WireLaws[List[IntList], BitVector, Err](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[collections.Adt, BitVector, Err]",
    WireLaws[Adt, BitVector, Err](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[collections.Cyclic, BitVector, Err]",
    WireLaws[Cyclic, BitVector, Err](mkWire).roundtrip
  )

  checkAll(
    "WireLaws[Adt1, BitVector, Err]+Adt2",
    WireLaws[Adt1, BitVector, Err](mkWire).roundtripCompat[Adt2]
  )
  checkAll(
    "WireLaws[IntList, BitVector, Err]+v2",
    WireLaws[IntList, BitVector, Err](mkWire).roundtripCompat[IntListV2]
  )
}

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
package circe

import cats.~>
import cats.implicits._

import io.circe._

import io.sigs.seals.laws.ArbInstances
import io.sigs.seals.laws.TestArbInstances
import io.sigs.seals.laws.TestTypes
import io.sigs.seals.laws.WireLaws
import io.sigs.seals.core.Wire

class CirceWireSpec
    extends tests.BaseLawsSpec
    with ArbInstances {

  import TestArbInstances.forTestData._
  import TestTypes.adts.defs.{ Adt1, Adt2 }
  import TestTypes.adts.recursive.IntList
  import TestTypes.adts.recursive.v2.{ IntList => IntListV2 }
  import TestTypes.collections.{ Adt, Cyclic }

  val mkWire = λ[Reified ~> Wire.Aux[?, Json, DecodingFailure]](
    x => Wires.wireFromReified(x)
  )

  checkAll(
    "WireLaws[Int, Json, DecodingFailure]",
    WireLaws[Int, Json, DecodingFailure](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[String, Json, DecodingFailure]",
    WireLaws[String, Json, DecodingFailure](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[Vector[String], Json, DecodingFailure]",
    WireLaws[Vector[String], Json, DecodingFailure](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[List[IntList], Json, DecodingFailure]",
    WireLaws[List[IntList], Json, DecodingFailure](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[collections.Adt, Json, DecodingFailure]",
    WireLaws[Adt, Json, DecodingFailure](mkWire).roundtrip
  )
  checkAll(
    "WireLaws[collections.Cyclic, Json, DecodingFailure]",
    WireLaws[Cyclic, Json, DecodingFailure](mkWire).roundtrip
  )

  checkAll(
    "WireLaws[Adt1, Json, DecodingFailure]+Adt2",
    WireLaws[Adt1, Json, DecodingFailure](mkWire).roundtripCompat[Adt2]
  )
  checkAll(
    "WireLaws[IntList, Json, DecodingFailure]+v2",
    WireLaws[IntList, Json, DecodingFailure](mkWire).roundtripCompat[IntListV2]
  )
}

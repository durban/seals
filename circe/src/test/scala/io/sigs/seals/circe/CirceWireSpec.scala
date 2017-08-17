/*
 * Copyright 2016-2017 Daniel Urban and contributors listed in AUTHORS
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

import cats.Eq

import io.circe._

class CirceWireSpec
    extends tests.BaseLawsSpec
    with laws.AbstractWireSpec[Json, DecodingFailure]
    with laws.ArbInstances {

  override def descE: String =
    "DecodingFailure"

  override def descR: String =
    "Json"

  override def equR: Eq[Json] =
    Json.eqJson

  override def shwE =
    DecodingFailure.showDecodingFailure

  override def mkWire[A](r: Reified[A]): Wire.Aux[A, Json, DecodingFailure] =
    Wires.wireFromReified(r)
}

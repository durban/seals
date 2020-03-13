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
package scodec

import cats.Eq

import _root_.scodec.bits.BitVector
import _root_.scodec.Err

class ScodecWireSpec
    extends tests.BaseLawsSpec
    with laws.AbstractWireSpec[BitVector, Err]
    with laws.ArbInstances {

  override def descE: String =
    "Err"

  override def descR: String =
    "BitVector"

  override def equR: Eq[BitVector] =
    _root_.scodec.interop.cats.BitVectorEqInstance

  override def shwE =
    _root_.scodec.interop.cats.ErrShowInstance

  override def mkWire[A](r: Reified[A]): Wire.Aux[A, BitVector, Err] =
    Wires.wireFromReified(r)
}

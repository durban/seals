/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
 * Copyright 2020 Nokia
 * SPDX-License-Identifier: Apache-2.0
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

import _root_.scodec.bits.BitVector
import _root_.scodec.{ Err => SErr }

object Wires extends Wires

trait Wires {

  implicit def wireFromReified[A](
    implicit A: Reified[A]
  ): Wire.Aux[A, BitVector, SErr] = new Wire[A] {
    type Repr = BitVector
    type Err = SErr

    override def toWire(a: A): Either[Err, BitVector] =
      Codecs.encoderFromReified(A).encode(a).toEither

    override def fromWire(r: BitVector): Either[Err, A] =
      Codecs.decoderFromReified(A).decode(r).toEither.map(_.value)

    override def reified = A
  }
}

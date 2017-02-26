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

import java.util.UUID

import cats.{ Eq, Show }

import scodec.bits.ByteVector

package object core {

  type EnumLike[A] = macros.EnumLike[A]
  val EnumLike = macros.EnumLike

  private[seals] implicit val symbolEq: Eq[Symbol] =
    Eq.fromUniversalEquals[Symbol]

  private[seals] implicit val uuidEq: Eq[UUID] =
    Eq.fromUniversalEquals[UUID]

  private[seals] implicit val byteVectorShow: Show[ByteVector] =
    Show.fromToString[ByteVector]

  private[seals] def impossible(msg: => String): Nothing =
    throw new AssertionError(msg)
}

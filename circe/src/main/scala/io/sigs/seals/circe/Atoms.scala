/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

import java.util.UUID

import cats.implicits._

import io.circe.numbers.BiggerDecimal

trait Atoms {

  implicit val atomicForBiggerDecimal: Atomic[BiggerDecimal] =
    Atoms.AtomicForBiggerDecimal
}

object Atoms extends Atoms {

  // Note: this is defined in the object (and
  // not the trait), because this way serialization
  // and deserialization of this Atomic instance
  // preserves object identity.
  private[Atoms] object AtomicForBiggerDecimal
      extends Atomic[BiggerDecimal]
      with Atomic.FallbackBinary[BiggerDecimal] {

    val description: String =
      "BiggerDecimal"

    val uuid: UUID =
      uuid"5c3d07fd-2f45-4f7b-af52-eeba62b95aa1"

    def stringRepr(a: BiggerDecimal): String =
      a.toString

    def fromString(s: String): Either[Atomic.Error, BiggerDecimal] = {
      BiggerDecimal.parseBiggerDecimal(s).toRight(
        left = Atomic.InvalidData(sh"not a BiggerDecimal: '${s}'")
      )
    }
  }
}

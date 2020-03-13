/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package circe

import io.circe.numbers.BiggerDecimal

import circe.Atoms._
import circe.Codecs._

class AtomsSpec extends BaseJsonSpec {

  "Atomic[BiggerDecimal]" in {
    val bds = List(
      BiggerDecimal.fromLong(0L),
      BiggerDecimal.NegativeZero,
      // this is too big for a BigDecimal:
      BiggerDecimal.parseBiggerDecimal(s"9999e${Int.MaxValue.toLong * 2}").getOrElse(fail)
    )
    val abd = Atomic[BiggerDecimal]
    for (bd <- bds) {
      val bd2 = checkJson(bd)
      bd2.isNegativeZero should === (bd.isNegativeZero)
      val bd3 = abd.fromBinary(abd.binaryRepr(bd)) match {
        case Right((d, _)) => d
        case x => fail(s"unexpected: ${x}")
      }
      bd3 should === (bd)
      bd3.isNegativeZero should === (bd.isNegativeZero)
    }
  }
}

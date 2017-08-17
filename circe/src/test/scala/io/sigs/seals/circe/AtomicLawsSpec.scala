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

import cats.Eq

import io.circe.numbers.BiggerDecimal

import org.scalacheck.{ Arbitrary, Gen }

class AtomicLawsSpec extends tests.BaseLawsSpec {

  import Atoms._

  implicit val eqBiggerDecimal: Eq[BiggerDecimal] =
    Eq.fromUniversalEquals

  implicit def arbBiggerDecimal(
    implicit
    arbBd: Arbitrary[BigDecimal],
    arbBi: Arbitrary[BigInt],
    arbDbl: Arbitrary[Double]
  ): Arbitrary[BiggerDecimal] = Arbitrary {
    Gen.oneOf(
      // fits into a BigDecimal:
      arbBd.arbitrary.map { x =>
        BiggerDecimal.fromBigDecimal(x.underlying)
      },
      // doesn't fit into a BigDecimal:
      arbBi.arbitrary.map { n =>
        val str = s"${n}e${n max Int.MaxValue.toLong + 1L}"
        BiggerDecimal.parseBiggerDecimal(str).getOrElse {
          core.impossible(s"cannot parse BiggerDecimal from '${str}'")
        }
      },
      // can contain negative zero:
      arbDbl.arbitrary.map(d => BiggerDecimal.fromDouble(- d))
    )
  }

  checkAtomicLaws[BiggerDecimal]("BiggerDecimal")
}

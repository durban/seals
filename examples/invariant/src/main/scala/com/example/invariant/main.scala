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

package com.example.invariant

import spire.math.Polynomial
import spire.std.float._
import spire.syntax.eq._

import io.circe._
import io.circe.syntax._

import io.sigs.seals.Reified
import io.sigs.seals.circe.Codec._

object Main extends App with PolySupport {

  /** x² + 2.0x + 4.0 */
  val original: Polynomial[Float] =
    Polynomial(Map(2 -> 1.0f, 1 -> 2.0f, 0 -> 4.0f))

  val serialized: Json =
    original.asJson

  val deserialized: Polynomial[Float] =
    serialized.as[Polynomial[Float]].getOrElse(???)

  implicitly[spire.algebra.Eq[Polynomial[Float]]]

  assert(original === deserialized)
  println(s"Original:     ${original}")
  println(s"Deserialized: ${deserialized}")
}

trait PolySupport {

  implicit val polyReified: Reified[Polynomial[Float]] = {
    Reified[List[(Int, Float)]].imap[Polynomial[Float]] { lst =>
      Polynomial[Float](lst.toMap)
    } { poly =>
      poly.terms.map(_.toTuple)
    }
  }
}

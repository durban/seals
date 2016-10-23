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

import shapeless.record._
import shapeless.union._

import _root_.scodec.bits._
import _root_.scodec.{ Attempt, DecodeResult }

import Codecs._

class CodecsSpec extends tests.BaseSpec {

  "Encoder from Reified" - {

    "Atoms" in {
      encoderFromReified[Int].encode(42) should === (
        Attempt.successful(hex"0000 0002 3432".bits)
      )
    }

    "Products" in {
      encoderFromReified[Record.`'a -> Int`.T].encode(Record(a = 42)) should === (
        Attempt.successful(hex"0000 0002 3432  A1".bits)
      )
    }

    "Sums" in {
      type U = Union.`'a -> Int, 'b -> Int`.T
      encoderFromReified[U].encode(Union[U](b = 42)) should === (
        Attempt.successful(hex"0000 0001 62  0000 0002 3432".bits)
      )
    }

    "Collections" in {
      encoderFromReified[Vector[Int]].encode(Vector(42, 43)) should === (
        Attempt.successful(hex"0000 0002  0000 0002 3432  0000 0002 3433".bits)
      )
    }
  }

  "Decoder from Reified" - {

    "Atoms" in {
      decoderFromReified[Int].decode(hex"0000 0002 3432   ffff".bits) should === (
        Attempt.successful(DecodeResult(42, hex"ffff".bits))
      )
    }

    "Products" in {
      decoderFromReified[Record.`'a -> Int`.T].decode(hex"0000 0002 3432  A1   abcd".bits) should === (
        Attempt.successful(DecodeResult(Record(a = 42), hex"abcd".bits))
      )
    }

    "Sums" in {
      type U = Union.`'a -> Int, 'b -> Int`.T
      decoderFromReified[U].decode(hex"0000 0001 62  0000 0002 3432   babe".bits) should === (
        Attempt.successful(DecodeResult(Union[U](b = 42), hex"babe".bits))
      )
    }

    "Collections" in {
      decoderFromReified[Vector[Int]].decode(hex"0000 0002  0000 0002 3432  0000 0002 3433   dead".bits) should === (
        Attempt.successful(DecodeResult(Vector(42, 43), hex"dead".bits))
      )
    }
  }
}

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
import _root_.scodec.{ Attempt, DecodeResult, Codec }

import Codecs._
import laws.TestTypes.{ CaseClass, Whatever }
import laws.TestTypes.adts.defs.Adt1
import laws.TestTypes.collections.WithList
import laws.TestInstances.atomic._

class CodecsSpec extends tests.BaseSpec {

  type U = Union.`'a -> Int, 'b -> Int`.T

  "Encoder from Reified" - {

    "Atoms" in {
      encoderFromReified[Int].encode(42) should === (
        Attempt.successful(hex"0000 0002 3432".bits)
      )
      encoderFromReified[Whatever.type].encode(Whatever) should === (
        Attempt.successful(hex"0000 0008 5768 6174 6576 6572".bits)
      )
    }

    "Products" in {
      encoderFromReified[Record.`'a -> Int`.T].encode(Record(a = 42)) should === (
        Attempt.successful(hex"0000 0002 3432  A1".bits)
      )
      encoderFromReified[CaseClass].encode(CaseClass(42L)) should === (
        Attempt.successful(hex"0000 0002 3432  A1".bits)
      )
    }

    "Sums" in {
      encoderFromReified[U].encode(Union[U](b = 42)) should === (
        Attempt.successful(hex"0000 0001 62  0000 0002 3432".bits)
      )
      encoderFromReified[Adt1].encode(Adt1.C(42)) should === (
        Attempt.successful(hex"0000 0001 43  0000 0002 3432  0000 0003 626F6F  A1".bits)
      )
    }

    "Collections" in {
      encoderFromReified[Vector[Int]].encode(Vector(42, 43)) should === (
        Attempt.successful(hex"0000 0002  0000 0002 3432  0000 0002 3433".bits)
      )
      encoderFromReified[WithList].encode(WithList(42, List(42.0f))) should === (
        Attempt.successful(hex"0000 0002 3432  0000 0001  0000 0004  3432 2E30  A1".bits)
      )
    }
  }

  "Decoder from Reified" - {

    "Atoms" in {
      decoderFromReified[Int].decode(hex"0000 0002 3432   ffff".bits) should === (
        Attempt.successful(DecodeResult(42, hex"ffff".bits))
      )
      decoderFromReified[Whatever.type].decode(hex"0000 0008 5768 6174 6576 6572   abcdef".bits) should === (
        Attempt.successful(DecodeResult(Whatever, hex"abcdef".bits))
      )
    }

    "Products" in {
      decoderFromReified[Record.`'a -> Int`.T].decode(hex"0000 0002 3432  A1   abcd".bits) should === (
        Attempt.successful(DecodeResult(Record(a = 42), hex"abcd".bits))
      )
      decoderFromReified[CaseClass].decode(hex"0000 0002 3432  A1".bits) should === (
        Attempt.successful(DecodeResult(CaseClass(42L), BitVector.empty))
      )
    }

    "Sums" in {
      decoderFromReified[U].decode(hex"0000 0001 62  0000 0002 3432   babe".bits) should === (
        Attempt.successful(DecodeResult(Union[U](b = 42), hex"babe".bits))
      )
      decoderFromReified[Adt1].decode(hex"0000 0001 43  0000 0002 3432  0000 0003 626F6F  A1   aaaa".bits) should === (
        Attempt.successful(DecodeResult(Adt1.C(42), hex"aaaa".bits))
      )
    }

    "Collections" in {
      decoderFromReified[Vector[Int]].decode(hex"0000 0002  0000 0002 3432  0000 0002 3433   dead".bits) should === (
        Attempt.successful(DecodeResult(Vector(42, 43), hex"dead".bits))
      )
      decoderFromReified[WithList].decode(hex"0000 0002 3432  0000 0001  0000 0004  3432 2E30  A1   ff".bits) should === (
        Attempt.successful(DecodeResult(WithList(42, List(42.0f)), hex"ff".bits))
      )
    }
  }

  "Roundtrip" - {

    "Implicit Codec from Reified" in {
      Codec.decode[Int](Codec.encode[Int](42).getOrElse(fail)).getOrElse(fail).value should === (42)
      Codec.decode[Whatever.type](Codec.encode[Whatever.type](Whatever).getOrElse(fail)).getOrElse(fail).value should === (Whatever)
      Codec.decode[Record.`'a -> Int`.T](Codec.encode[Record.`'a -> Int`.T](Record(a = 42)).getOrElse(fail)).getOrElse(fail).value should === (Record(a = 42))
      Codec.decode[CaseClass](Codec.encode[CaseClass](CaseClass(42L)).getOrElse(fail)).getOrElse(fail).value should === (CaseClass(42L))
      Codec.decode[U](Codec.encode[U](Union[U](b = 42)).getOrElse(fail)).getOrElse(fail).value should === (Union[U](b = 42))
      Codec.decode[Adt1](Codec.encode[Adt1](Adt1.C(42)).getOrElse(fail)).getOrElse(fail).value should === (Adt1.C(42))
      Codec.decode[Vector[Int]](Codec.encode[Vector[Int]](Vector(42, 43)).getOrElse(fail)).getOrElse(fail).value should === (Vector(42, 43))
      Codec.decode[WithList](Codec.encode[WithList](WithList(42, List(42.0f))).getOrElse(fail)).getOrElse(fail).value should === (WithList(42, List(42.0f)))
    }

    "Models" in {
      val mod: Model = Reified[U].model
      Codec.decode[Model](Codec.encode(mod).getOrElse(fail)).getOrElse(fail).value should === (mod)
    }

    "Envelopes" in {
      val env: Envelope[U] = Envelope(Union[U](b = 42))
      Codec.decode[Envelope[U]](Codec.encode(env).getOrElse(fail)).getOrElse(fail).value should === (env)
    }
  }
}

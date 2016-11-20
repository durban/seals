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

import laws.TestTypes.adts.defs.{ Adt1, Adt2 }

import _root_.scodec.stream.codec.StreamCodec
import _root_.scodec.stream.decode.DecodingError

class StreamCodecsSpec extends tests.BaseSpec {

  import StreamCodecs._

  val data1 = Vector[Adt1](Adt1.C(42), Adt1.Dummy, Adt1.Dummy, Adt1.C(42, "qwerty"))
  val data2 = Vector[Adt2](Adt2.C(42), Adt2.Dummy, Adt2.Dummy, Adt2.C(42, "qwerty"))

  "Streaming" - {

    "roundtrip" in {
      val bits = StreamCodec[Adt1].encodeAllValid(data1)
      // TODO: make it work with Adt2
      StreamCodec[Adt1].decodeAllValid(bits) should === (data1)
    }

    "incompatible models" in {
      val bits = StreamCodec[Adt1].encodeAllValid(data1)
      val ex = intercept[DecodingError] {
        StreamCodec[Int].decodeAllValid(bits)
      }
      ex.err.message should include ("incompatible models")
    }
  }
}

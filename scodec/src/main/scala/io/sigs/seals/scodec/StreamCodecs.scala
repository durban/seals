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

import fs2.Pull

import _root_.scodec.Err
import _root_.scodec.bits.BitVector
import _root_.scodec.stream.{ StreamEncoder, StreamDecoder, StreamCodec, decode, encode }

object StreamCodecs extends StreamCodecs

trait StreamCodecs {

  implicit def streamCodecFromReified[A](implicit A: Reified[A]): StreamCodec[A] =
    StreamCodec.instance(streamEncoderFromReified(A), streamDecoderFromReified(A))

  def streamEncoderFromReified[A](implicit A: Reified[A]): StreamEncoder[A] = {
    Codecs.encoderFromReified[Model].encode(A.model).fold(
      err => { encode.fail(err) },
      bv => {
        emit[A](bv) ++ encode.many[A](Codecs.encoderFromReified(A))
      }
    )
  }

  /** See https://github.com/scodec/scodec-stream/issues/12 */
  private[this] def emit[A](bits: BitVector): StreamEncoder[A] = {
    StreamEncoder.instance[A] { h =>
      Pull.output1(bits) >> Pull.pure(h -> encode.empty[A])
    }
  }

  def streamDecoderFromReified[A](implicit A: Reified[A]): StreamDecoder[A] = {
    decode.once(Codecs.decoderFromReified[Model]).flatMap { model =>
      if (model compatible A.model) {
        // TODO: manyChunked?
        decode.many(Codecs.decoderFromReified(A))
      } else {
        decode.fail(Err(s"incompatible models: expected '${A.model}', got '${model}'"))
      }
    }
  }
}

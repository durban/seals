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
package scodec

import fs2.{ Pull, Pipe, Handle }

import _root_.scodec.{ Err, Attempt, Decoder, DecodeResult }
import _root_.scodec.bits.BitVector
import _root_.scodec.stream.{ StreamEncoder, StreamDecoder, StreamCodec, decode, encode }
import decode.DecodingError

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
        decode.fail(Err(sh"incompatible models: expected '${A.model}', got '${model}'"))
      }
    }
  }

  def pipe[F[_], A](implicit A: Reified[A]): Pipe[F, BitVector, A] = {

    def decodeOne[R](h: Handle[F, BitVector], decoder: Decoder[R]) = {
      def go(buff: BitVector, h: Handle[F, BitVector]): Pull[F, Nothing, (R, Handle[F, BitVector])] = {
        for {
          (bv, h) <- h.await1
          h <- {
            val data = buff ++ bv
            decoder.decode(data) match {
              case Attempt.Failure(err: Err.InsufficientBits) =>
                go(data, h)
              case Attempt.Failure(err) =>
                Pull.fail(DecodingError(err))
              case Attempt.Successful(DecodeResult(a, rem)) =>
                Pull.pure((a, h.push1(rem)))
            }
          }
        } yield h
      }
      go(BitVector.empty, h)
    }

    def decodeMany[R](decoder: Decoder[R]): Handle[F, BitVector] => Pull[F, R, Nothing] = {
      Pull.loop[F, R, Handle[F, BitVector]] { h =>
        for {
          (r, h) <- decodeOne[R](h, decoder)
          _ <- Pull.output1(r)
        } yield h
      }
    }

    def decode(h: Handle[F, BitVector]): Pull[F, A, Handle[F, BitVector]] = {
      decodeOne[Model](h, Codecs.decoderFromReified[Model]).flatMap { case (model, h) =>
        if (model compatible A.model) {
          decodeMany(Codecs.decoderFromReified[A])(h)
        } else {
          Pull.fail(DecodingError(Err(sh"incompatible models: expected '${A.model}', got '${model}'")))
        }
      }
    }

    _.pull(decode)
  }
}

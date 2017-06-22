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

import fs2.{ Stream, Pull, Pipe }

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
      Pull.output1(bits) >> Pull.pure(Some(h -> encode.empty[A]))
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

    def decodeOne[R](s: Stream[F, BitVector], decoder: Decoder[R]): Pull[F, Nothing, Option[(R, Stream[F, BitVector])]] = {
      def go(buff: BitVector, s: Stream[F, BitVector]): Pull[F, Nothing, Option[(R, Stream[F, BitVector])]] = {
        for {
          opt <- s.pull.uncons1
          res <- opt match {
            case Some((bv, rest)) =>
              val data = buff ++ bv
              decoder.decode(data) match {
                case Attempt.Failure(Err.InsufficientBits(_, _, _)) =>
                  go(data, rest)
                case Attempt.Failure(err) =>
                  Pull.fail(DecodingError(err)) : Pull[F, Nothing, Option[(R, Stream[F, BitVector])]]
                case Attempt.Successful(DecodeResult(a, rem)) =>
                  Pull.pure(Some((a, Stream(rem) ++ rest)))
              }
            case None =>
              Pull.pure(None)
          }
        } yield res
      }

      go(BitVector.empty, s)
    }

    def decodeMany[R](decoder: Decoder[R]): Stream[F, BitVector] => Pull[F, R, Option[Stream[F, BitVector]]] = {
      Pull.loop[F, R, Stream[F, BitVector]] { s =>
        for {
          opt <- decodeOne[R](s, decoder)
          cont <- opt match {
            case Some((r, tail)) =>
              Pull.output1(r) >> Pull.pure(Some(tail))
            case None =>
              Pull.pure(None)
          }
        } yield cont
      }
    }

    def decode(s: Stream[F, BitVector]): Pull[F, A, Option[Stream[F, BitVector]]] = {
      decodeOne[Model](s, Codecs.decoderFromReified[Model]).flatMap {
        case Some((model, tail)) =>
          if (model compatible A.model) {
            decodeMany(Codecs.decoderFromReified[A])(tail)
          } else {
            Pull.fail(DecodingError(Err(sh"incompatible models: expected '${A.model}', got '${model}'")))
          }
        case None =>
          Pull.fail(DecodingError(Err("invalid stream: no model header found")))
      }
    }

    s => {
      decode(s).stream
    }
  }
}

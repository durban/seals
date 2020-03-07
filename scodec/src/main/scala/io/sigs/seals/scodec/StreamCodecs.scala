/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.implicits._

import fs2.{ Stream, Pull, Pipe, RaiseThrowable }

import _root_.scodec.{ Err, Attempt, Decoder, DecodeResult }
import _root_.scodec.bits.BitVector
import _root_.scodec.stream.{ StreamEncoder, StreamDecoder }
import _root_.scodec.stream.CodecError

object StreamCodecs extends StreamCodecs

trait StreamCodecs {

  // TODO: implicit?
  def streamEncoderFromReified[A](implicit A: Reified[A]): StreamEncoder[A] = {
    Codecs.encoderFromReified[Model].encode(A.model).fold(
      err => { StreamEncoder.raiseError(err) },
      bv => {
        StreamEncoder.emit[A](bv) ++ StreamEncoder.many[A](Codecs.encoderFromReified(A))
      }
    )
  }

  // TODO: implicit?
  def streamDecoderFromReified[A](implicit A: Reified[A]): StreamDecoder[A] = {
    StreamDecoder.once(Codecs.decoderFromReified[Model]).flatMap { model =>
      if (model compatible A.model) {
        // TODO: manyChunked?
        StreamDecoder.many(Codecs.decoderFromReified(A))
      } else {
        StreamDecoder.raiseError(Err(sh"incompatible models: expected '${A.model}', got '${model}'"))
      }
    }
  }

  def pipe[F[_], A](implicit A: Reified[A], F: RaiseThrowable[F]): Pipe[F, BitVector, A] = {

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
                  Pull.raiseError(CodecError(err)) : Pull[F, Nothing, Option[(R, Stream[F, BitVector])]]
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

    def decodeMany[R](decoder: Decoder[R]): Stream[F, BitVector] => Pull[F, R, Unit] = {
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
      }.map(_.void) // Pull.loop always finishes with None, so we can ignore the result
    }

    def decode(s: Stream[F, BitVector]): Pull[F, A, Unit] = {
      decodeOne[Model](s, Codecs.decoderFromReified[Model]).flatMap {
        case Some((model, tail)) =>
          if (model compatible A.model) {
            decodeMany(Codecs.decoderFromReified[A])(tail)
          } else {
            Pull.raiseError(CodecError(Err(sh"incompatible models: expected '${A.model}', got '${model}'")))
          }
        case None =>
          Pull.raiseError(CodecError(Err("invalid stream: no model header found")))
      }
    }

    s => {
      decode(s).stream
    }
  }
}

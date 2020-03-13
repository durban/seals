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

package dev.tauri.seals
package scodec

import java.nio.charset.StandardCharsets

import cats.implicits._
import cats.{ Traverse, Monad }

import _root_.scodec.{ Codec, Encoder, Decoder, Attempt, SizeBound, DecodeResult, Err }
import _root_.scodec.codecs.{
  int32,
  uint8,
  vlong,
  vint,
  bits,
  constant,
  string,
  discriminated,
  variableSizeBytes,
  variableSizeBytesLong
}
import _root_.scodec.bits._
import _root_.scodec.interop.cats._

import core.symbolEq

object Codecs extends Codecs

trait Codecs {

  private sealed abstract class FieldOrEnd(val marker: Int)
  private final case object Field extends FieldOrEnd(fieldMarker)
  private final case object End extends FieldOrEnd(endMarker)

  private[this] final val fieldMarker = 0xA2
  private[this] final val endMarker = 0xA1

  private[this] val symbolCodec: Codec[Symbol] =
    variableSizeBytes(vint, string(StandardCharsets.UTF_8))
      .withToString("Symbol")
      .xmap(Symbol.apply, _.name)

  private[this] val fieldLengthCodec: Codec[Long] =
    vlong

  private[this] val lengthPrefixField: Codec[BitVector] =
    variableSizeBytesLong(fieldLengthCodec, bits)

  private[this] val emptyCodec: Codec[Unit] =
    constant(BitVector.empty)

  private[this] val fieldOrEnd = {
    discriminated[FieldOrEnd].by(uint8).
      | (Field.marker) { case Field => () } (_ => Field) (emptyCodec).
      | (End.marker) { case End => () } (_ => End) (emptyCodec)
  }

  implicit def codecFromReified[A](implicit A: Reified[A]): Codec[A] =
    Codec(encoderFromReified[A], decoderFromReified[A])

  def encoderFromReified[A](implicit A: Reified[A]): Encoder[A] = new Encoder[A] {

    override def encode(value: A): Attempt[BitVector] = {
      A.foldClose(value)(Reified.Folder.simple[Attempt[BitVector]](
        atom = a => Attempt.successful(a.binaryRepr.bits),
        hNil = () => fieldOrEnd.encode(End),
        hCons = (_, h, t) => for {
          fv <- fieldOrEnd.encode(Field)
          hv <- h
          hv <- lengthPrefixField.encode(hv)
          tv <- t
        } yield fv ++ hv ++ tv,
        sum = (l, v) => for {
          lv <- symbolCodec.encode(l)
          vv <- v
        } yield lv ++ vv,
        vector = els => for {
          vec <- Traverse[Vector].sequence(els)
          len <- int32.encode(vec.length)
        } yield len ++ BitVector.concat(vec)
      ))
    }

    override def sizeBound: SizeBound =
      SizeBound.unknown
  }

  def decoderFromReified[A](implicit A: Reified[A]): Decoder[A] = new Decoder[A] {
    override def decode(bits: BitVector): Attempt[DecodeResult[A]] = {
      val x = A.unfold(Reified.Unfolder.instance[BitVector, Err, Int](
        atom = { b =>
          val bv = b.bytes
          val padding = (bv.length * 8L) - b.length
          Right(Reified.BinaryResult(bv, _.bits.dropRight(padding)))
        },
        atomErr = {
          case (_, Atomic.InsufficientData(exp, act)) =>
            Err.insufficientBits(needed = exp, have = act)
          case (_, Atomic.InvalidData(msg)) =>
            Err.apply(sh"error while decoding atom: '${msg}'")
        },
        hNil = { b =>
          // we may have to skip fields:
          Monad[Attempt].tailRecM(b) { b: BitVector =>
            fieldOrEnd.decode(b).fold(
              err => Attempt.failure(err),
              dr => dr.value match {
                case End =>
                  // OK, this is HNil, we're done:
                  Attempt.successful(Right(dr.remainder))
                case Field =>
                  // we have a field, skip it and continue:
                  lengthPrefixField.decode(dr.remainder).map { dr =>
                    Left(dr.remainder)
                  }
              }
            )
          }.toEither
        },
        hCons = { (b, l) =>
          fieldOrEnd.decode(b).fold(
            err => Either.right(Either.left(err)),
            dr => dr.value match {
              case End =>
                Either.left(Err(sh"missing field: '${l.name}'"))
              case Field =>
                Either.right(fieldLengthCodec.decode(dr.remainder).toEither.map(dr => (dr.remainder, Right(_))))
            }
          )
        },
        cNil = { _ => Err("no variant matched (CNil reached)") },
        cCons = { (b, l) =>
          symbolCodec.decode(b).map { dr =>
            if (dr.value === l) {
              Left(dr.remainder)
            } else {
              Right(b)
            }
          }.toEither
        },
        vectorInit = { b =>
          int32.decode(b).map { len =>
            (len.remainder, len.value)
          }.toEither
        },
        vectorFold = { (b: BitVector, len: Int) =>
          if (len > 0) Either.right(Some((b, len - 1)))
          else Either.right(None)
        },
        unknownError = Err(_)
      ))(bits)

      x.map { case (value, remainder) => DecodeResult(value, remainder) }.toAttempt
    }
  }
}

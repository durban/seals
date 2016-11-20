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

import cats.implicits._
import cats.Traverse

import _root_.scodec.{ Codec, Encoder, Decoder, Attempt, SizeBound, DecodeResult, Err }
import _root_.scodec.codecs.{ int32, uint8, utf8_32, constant, discriminated }
import _root_.scodec.bits._
import _root_.scodec.interop.cats._

import core.symbolEq

object Codecs {

  private[this] final val hnilConst = 0xA1
  private[this] final val fieldConst = 0xA2

  private[this] val symbolCodec: Codec[Symbol] =
    utf8_32.xmap(Symbol.apply, _.name)

  private[this] val hnilCodec: Codec[Unit] =
    constant(uint8.encode(hnilConst).getOrElse(throw new AssertionError))

  private[this] val fieldSignCodec: Codec[Unit] =
    constant(uint8.encode(fieldConst).getOrElse(throw new AssertionError))

  private[this] val emptyCodec: Codec[Unit] =
    constant(BitVector.empty)

  private[this] val fieldDiscriminator = {
    discriminated[Either[Unit, Unit]].by(uint8).
      | (hnilConst) { case Left(l) => l } (Left.apply) (emptyCodec).
      | (fieldConst) { case Right(r) => r } (Right.apply) (emptyCodec)
  }

  implicit def codecFromReified[A](implicit A: Reified[A]): Codec[A] =
    Codec(encoderFromReified[A], decoderFromReified[A])

  def encoderFromReified[A](implicit A: Reified[A]): Encoder[A] = new Encoder[A] {

    override def encode(value: A): Attempt[BitVector] = {
      A.foldClose(value)(Reified.Folder.simple[Attempt[BitVector]](
        atom = utf8_32.encode,
        hNil = () => hnilCodec.encode(()),
        hCons = (_, h, t) => for {
          fv <- fieldSignCodec.encode(())
          hv <- h
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
        atom = { b => utf8_32.decode(b).map(x => (x.value, x.remainder)).toEither },
        atomErr = { _ => Err("cannot decode atom") },
        hNil = { b => hnilCodec.decode(b).map(_.remainder).toEither },
        hCons = { (b, l) =>
          fieldDiscriminator.decode(b).fold(
            err => Either.right(Either.left(err)),
            dr => dr.value match {
              case Left(_) =>
                Either.left(Err(s"missing field: '${l.name}'"))
              case Right(_) =>
                Either.right(Either.right((dr.remainder, Right.apply)))
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

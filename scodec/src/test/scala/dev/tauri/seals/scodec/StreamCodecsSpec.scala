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

import laws.TestTypes.adts.defs.{ Adt1, Adt2 }

import cats.effect.IO

import fs2.{ Stream, Pure, Pipe, Pull }

import _root_.scodec.bits._
import _root_.scodec.stream.CodecError

import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import org.scalacheck.Gen

class StreamCodecsSpec extends tests.BaseSpec with ScalaCheckDrivenPropertyChecks {

  import StreamCodecs._

  val data1 = Vector[Adt1](Adt1.C(42), Adt1.Dummy, Adt1.Dummy, Adt1.C(42, "qwerty"))
  val data2 = Vector[Adt2](Adt2.C(42), Adt2.Dummy, Adt2.Dummy, Adt2.C(42, "qwerty"))

  "Streaming" - {

    "roundtrip" in {
      val bits = streamEncoderFromReified[Adt1].encodeAllValid(data1)
      streamDecoderFromReified[Adt2].decode(Stream(bits).covary[IO]).compile.toVector.unsafeRunSync() should === (data2)
    }

    "incompatible models" in {
      val bits = streamEncoderFromReified[Adt1].encodeAllValid(data1)
      val ex = intercept[CodecError] {
        streamDecoderFromReified[Int].decode(Stream(bits).covary[IO]).compile.toVector.unsafeRunSync()
      }
      ex.err.message should include ("incompatible models")
    }

    "pipe" in {
      val bits = streamEncoderFromReified[Adt1].encodeAllValid(data1)
      forAll(genStream(bits)) { src: Stream[Pure, BitVector] =>
        src.through(StreamCodecs.pipe[IO, Adt2]).compile.toVector.unsafeRunSync() should === (data2)
        val ex = intercept[CodecError] {
          src.through(StreamCodecs.pipe[IO, Int]).compile.toVector.unsafeRunSync()
        }
        ex.err.message should include ("incompatible models")
      }
      forAll(genTaskStream(bits)) { src: Stream[IO, BitVector] =>
        src.through(StreamCodecs.pipe[IO, Adt2]).compile.toVector.unsafeRunSync() should === (data2)
        val ex = intercept[CodecError] {
          src.through(StreamCodecs.pipe[IO, Int]).compile.toVector.unsafeRunSync()
        }
        ex.err.message should include ("incompatible models")
      }
    }
  }

  def genStream(data: BitVector): Gen[Stream[Pure, BitVector]] =
    slice(data).map(_.through(randomizeChunks(chunkSize)))

  def genTaskStream(data: BitVector): Gen[Stream[IO, BitVector]] =
    genStream(data).map(taskify)

  def randomizeChunks[F[_], A](chunkSize: Gen[Int]): Pipe[F, A, A] = {

    def doIt(tp: Stream.ToPull[F, A]): Pull[F, A, Option[Stream[F, A]]] = {
      val n = chunkSize.sample.getOrElse(fail("cannot generate chunk size"))
      for {
        opt <- tp.unconsN(n, allowFewer = true)
        rest <- opt match {
          case Some((seg, rest)) =>
            Pull.output(seg) >> Pull.pure(Some(rest))
          case None =>
            Pull.pure(None)
        }
      } yield rest
    }

    _.repeatPull(doIt)
  }

  def slice(bits: BitVector): Gen[Stream[Pure, BitVector]] = {
    if (bits.isEmpty) {
      Gen.const(Stream.empty)
    } else {
      for {
        n <- chunkSize
        (bv, rest) = (bits.take(n.toLong), bits.drop(n.toLong))
        rest <- slice(rest)
      } yield Stream(bv) ++ rest
    }
  }

  val chunkSize: Gen[Int] = Gen.oneOf(
    Gen.choose(0, 10),
    Gen.choose(11, 100),
    Gen.choose(101, Int.MaxValue)
  )

  def taskify[A](s: Stream[Pure, A]): Stream[IO, A] = {
    s.toVector.foldLeft(Stream.empty : Stream[IO, A]) { (stream, a) =>
      stream ++ Stream.eval(IO {
        Thread.sleep(10)
        a
      })
    }
  }
}

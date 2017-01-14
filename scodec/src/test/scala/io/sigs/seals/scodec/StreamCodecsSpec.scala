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

import fs2.{ Stream, Pure, Pipe, Handle, Pull, Chunk, Task }

import _root_.scodec.bits._
import _root_.scodec.stream.codec.StreamCodec
import _root_.scodec.stream.decode.DecodingError

import org.scalatest.prop.GeneratorDrivenPropertyChecks

import org.scalacheck.Gen

class StreamCodecsSpec extends tests.BaseSpec with GeneratorDrivenPropertyChecks {

  import StreamCodecs._

  val data1 = Vector[Adt1](Adt1.C(42), Adt1.Dummy, Adt1.Dummy, Adt1.C(42, "qwerty"))
  val data2 = Vector[Adt2](Adt2.C(42), Adt2.Dummy, Adt2.Dummy, Adt2.C(42, "qwerty"))

  "Streaming" - {

    "roundtrip" in {
      val bits = StreamCodec[Adt1].encodeAllValid(data1)
      StreamCodec[Adt2].decodeAllValid(bits) should === (data2)
    }

    "incompatible models" in {
      val bits = StreamCodec[Adt1].encodeAllValid(data1)
      val ex = intercept[DecodingError] {
        StreamCodec[Int].decodeAllValid(bits)
      }
      ex.err.message should include ("incompatible models")
    }

    // TODO:
    "pipe" ignore {
      val bits = StreamCodec[Adt1].encodeAllValid(data1)
      forAll(genStream(bits)) { src: Stream[Pure, BitVector] =>
        src.through(StreamCodecs.pipe[Pure, Adt2]).toVector should === (data2)
        val ex = intercept[DecodingError] {
          src.through(StreamCodecs.pipe[Pure, Int]).toVector
        }
        ex.err.message should include ("incompatible models")
      }
      forAll(genTaskStream(bits)) { src: Stream[Task, BitVector] =>
        src.through(StreamCodecs.pipe[Task, Adt2]).runLog.unsafeRun() should === (data2)
        val ex = intercept[DecodingError] {
          src.through(StreamCodecs.pipe[Task, Int]).runLog.unsafeRun()
        }
        ex.err.message should include ("incompatible models")
      }
    }
  }

  def genStream(data: BitVector): Gen[Stream[Pure, BitVector]] =
    slice(data).map(_.through(randomizeChunks(chunkSize)))

  def genTaskStream(data: BitVector): Gen[Stream[Task, BitVector]] =
    genStream(data).map(taskify)

  def randomizeChunks[F[_], A](chunkSize: Gen[Int]): Pipe[F, A, A] = {

    def doIt(h: Handle[F, A]): Pull[F, A, Handle[F, A]] = {
      val n = chunkSize.sample.getOrElse(fail("cannot generate chunk size"))
      for {
        (cs, h) <- h.awaitN(n, allowFewer = true)
        _ <- Pull.output(Chunk.concat(cs))
      } yield h
    }

    _.repeatPull(doIt)
  }

  def slice(bits: BitVector): Gen[Stream[Pure, BitVector]] = {
    if (bits.isEmpty) {
      Gen.const(Stream.empty[Pure, BitVector])
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

  def taskify[A](s: Stream[Pure, A]): Stream[Task, A] = {
    s.toVector.foldLeft(Stream.empty[Task, A]) { (stream, a) =>
      stream ++ Stream.eval(Task.delay {
        Thread.sleep(10)
        a
      })
    }
  }
}

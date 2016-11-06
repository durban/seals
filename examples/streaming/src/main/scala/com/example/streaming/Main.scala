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

package com.example.streaming

import java.io.{ InputStream, OutputStream, FileInputStream, FileOutputStream }
import java.nio.file.{ Path, Paths }

import fs2.{ Stream, Chunk, Task, Pure, Pull }

import scodec.bits.BitVector
import scodec.{ Codec, Err, Attempt }
import scodec.stream.{ encode, decode, StreamDecoder, StreamEncoder }

import io.sigs.seals.{ Model, Reified }
import io.sigs.seals.scodec.Codecs._

object Main {

  sealed trait Color
  final case object Brown extends Color
  final case object Grey extends Color

  sealed trait Animal
  final case class Elephant(name: String, tuskLength: Float) extends Animal
  final case class Quokka(name: String, color: Color = Brown) extends Animal
  final case class Quagga(name: String, speed: Double) extends Animal

  def decoder(implicit r: Reified[Animal]): StreamDecoder[Animal] = {
    decode.once(Codec[Model]).flatMap { model =>
      if (model compatible r.model) {
        decode.many(Codec[Animal])
      } else {
        decode.fail(Err(s"incompatible models: expected '${r.model}', got '${model}'"))
      }
    }
  }

  def encoder(implicit r: Reified[Animal]): StreamEncoder[Animal] = {
    def emit[A](bits: BitVector): StreamEncoder[A] = {
      StreamEncoder.instance[A] { h =>
        Pull.output1(bits) >> Pull.pure(h -> encode.empty[A])
      }
    }
    Codec[Model].encode(r.model) match {
      case Attempt.Successful(bv) =>
        emit[Animal](bv) ++
        encode.many(Codec[Animal])
      case Attempt.Failure(err) =>
        encode.fail(err)
    }
  }

  def transform(from: InputStream, to: OutputStream)(f: Animal => Stream[Pure, Animal])(
    implicit r: Reified[Animal]
  ): Task[Unit] = {
    val sIn: Stream[Task, Animal] = decoder.decodeInputStream(from).flatMap(f)
    val sOut: Stream[Task, Unit] = encoder.encode(sIn).mapChunks[Byte] { bvs =>
      Chunk.bytes(bvs.foldLeft(BitVector.empty)(_ ++ _).toByteArray)
    }.to(fs2.io.writeOutputStream(Task.now(to)))
    sOut.run
  }

  val transformer: Animal => Stream[Pure, Animal] = {
    case Elephant(n, tl) => Stream(Elephant(n, tl + 17))
    case Quokka(n, Brown) => Stream(Quokka(n, Grey))
    case q @ Quokka(_, _) => Stream(q)
    case Quagga(_, _) => Stream.empty
  }

  def main(args: Array[String]): Unit = {
    val (from, to) = args match {
      case Array(from, to, _*) =>
        (from, to)
      case Array(from) =>
        (from, "out.bin")
      case _ =>
        ("in.bin", "out.bin")
    }

    val task = transform(new FileInputStream(from), new FileOutputStream(to))(transformer)
    task.unsafeRun()
  }
}

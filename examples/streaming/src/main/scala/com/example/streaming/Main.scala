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

import fs2.{ Stream, Chunk, Task, Pure }

import scodec.bits.BitVector
import scodec.stream.StreamCodec

import io.sigs.seals.scodec.StreamCodecs._

object Main {

  sealed trait Color
  final case object Brown extends Color
  final case object Grey extends Color

  sealed trait Animal
  final case class Elephant(name: String, tuskLength: Float) extends Animal
  final case class Quokka(name: String, color: Color = Brown) extends Animal
  final case class Quagga(name: String, speed: Double) extends Animal

  def transform(from: InputStream, to: OutputStream)(f: Animal => Stream[Pure, Animal]): Task[Unit] = {
    val sIn: Stream[Task, Animal] = StreamCodec[Animal].decodeInputStream(from).flatMap(f)
    val sOut: Stream[Task, Unit] = StreamCodec[Animal].encode(sIn).mapChunks[Byte] { bvs =>
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

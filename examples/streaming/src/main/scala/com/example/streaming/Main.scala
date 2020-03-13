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

package com.example.streaming

import java.io.{ InputStream, OutputStream, FileInputStream, FileOutputStream }

import cats.implicits._
import cats.effect.{ IO, IOApp, Blocker, ExitCode }

import fs2.{ Stream, Chunk, Pure }

import dev.tauri.seals.scodec.StreamCodecs._

object Main extends IOApp {

  sealed trait Color
  final case object Brown extends Color
  final case object Grey extends Color

  sealed trait Animal
  final case class Elephant(name: String, tuskLength: Float) extends Animal
  final case class Quokka(name: String, color: Color = Brown) extends Animal
  final case class Quagga(name: String, speed: Double) extends Animal

  def transform(from: InputStream, to: OutputStream)(f: Animal => Stream[Pure, Animal]): IO[Unit] = {
    Blocker[IO].use { blocker =>
      val input = fs2.io.readInputStream(
        IO.pure(from),
        chunkSize = 1024,
        blocker = blocker
      )
      val sIn: Stream[IO, Animal] = input.through(streamDecoderFromReified[Animal].toPipeByte[IO]).flatMap(f)
      val sOut: Stream[IO, Unit] = streamEncoderFromReified[Animal].encode(sIn).flatMap { bv =>
        Stream.chunk(Chunk.bytes(bv.bytes.toArray))
      }.through(fs2.io.writeOutputStream(
        IO.pure(to),
        blocker = blocker,
        closeAfterUse = true
      ))
      sOut.compile.drain
    }
  }

  val transformer: Animal => Stream[Pure, Animal] = {
    case Elephant(n, tl) => Stream(Elephant(n, tl + 17))
    case Quokka(n, Brown) => Stream(Quokka(n, Grey))
    case q @ Quokka(_, _) => Stream(q)
    case Quagga(_, _) => Stream.empty
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val (from, to) = args match {
      case List(from, to, _*) =>
        (from, to)
      case List(from) =>
        (from, "out.bin")
      case _ =>
        ("in.bin", "out.bin")
    }

    val task = transform(new FileInputStream(from), new FileOutputStream(to))(transformer)
    task.as(ExitCode.Success)
  }
}

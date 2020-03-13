/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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

package com.example.server

import java.net.{ InetSocketAddress, InetAddress }
import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.Executors

import scala.concurrent.duration._

import cats.implicits._
import cats.effect.{ IO, IOApp, ExitCode, Resource, Blocker }

import fs2.{ Stream, Chunk }
import fs2.io.tcp

import scodec.bits.BitVector
import scodec.Codec

import dev.tauri.seals.scodec.Codecs._

import com.example.proto._

object Server extends IOApp {

  final val bufferSize = 32 * 1024
  final val timeout = Some(2.seconds)
  final val maxClients = 200
  final val port = 8080

  val rnd = new scala.util.Random

  def addr(port: Int): InetSocketAddress =
    new InetSocketAddress(InetAddress.getLoopbackAddress, port)

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { bl =>
      tcp.SocketGroup[IO](bl).use { sg =>
        serve(port, sg).compile.drain.as(ExitCode.Success)
      }
    }
  }

  def serve(port: Int, sg: tcp.SocketGroup): Stream[IO, Unit] = {
    Stream.resource(sg.serverResource[IO](addr(port))).flatMap {
      case (localAddr, sockets) =>
        val s = sockets.map { socket =>
          Stream.resource(socket).flatMap { socket =>
            val bvs: Stream[IO, BitVector] = socket.reads(bufferSize, timeout).chunks.map(ch => BitVector.view(ch.toArray))
            val tsk: IO[BitVector] = bvs.compile.toVector.map(_.foldLeft(BitVector.empty)(_ ++ _))
            val request: IO[Request] = tsk.flatMap { bv =>
              Codec[Request].decode(bv).fold(
                err => IO.raiseError(new Exception(err.toString)),
                result => IO.pure(result.value)
              )
            }
            val response: IO[Response] = request.flatMap(logic)
            val encoded: Stream[IO, Byte] = Stream.eval(response)
              .map(r => Codec[Response].encode(r).require)
              .flatMap { bv => Stream.chunk(Chunk.bytes(bv.bytes.toArray)) }
            encoded.through(socket.writes(timeout)).onFinalize(socket.endOfOutput)
          }
        }
        s.parJoin[IO, Unit](maxClients)
    }
  }

  def logic(req: Request): IO[Response] = req match {
    case RandomNumber(min, max) =>
      if (min < max) {
        IO {
          val v = rnd.nextInt(max - min + 1) + min
          Number(v)
        }
      } else if (min === max) {
        IO.pure(Number(min))
      } else {
        IO.raiseError(new IllegalArgumentException("min must not be greater than max"))
      }
    case ReSeed(s) =>
      IO {
        rnd.setSeed(s)
        Ok
      }
  }
}

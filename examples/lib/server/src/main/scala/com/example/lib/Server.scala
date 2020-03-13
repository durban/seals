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

package com.example.lib

import java.net.{ InetSocketAddress, InetAddress }

import scala.concurrent.duration._

import cats.implicits._
import cats.effect.{ IO, IOApp, ExitCode, Blocker }

import fs2.{ Stream, Chunk }
import fs2.io.tcp

import scodec.bits.BitVector
import scodec.stream.{ StreamEncoder, StreamDecoder }

import dev.tauri.seals.scodec.StreamCodecs._
import dev.tauri.seals.scodec.StreamCodecs.{ pipe => decPipe }

import Protocol.v1.{ Request, Response, Random, Seed, RandInt, Seeded }

object Server extends IOApp {

  final val bufferSize = 32 * 1024
  final val timeout = Some(2.seconds)
  final val maxClients = 200

  val rnd = new scala.util.Random

  def addr(port: Int): InetSocketAddress =
    new InetSocketAddress(InetAddress.getLoopbackAddress, port)

  val reqCodec: StreamDecoder[Request] = streamDecoderFromReified[Request]
  val resCodec: StreamEncoder[Response] = streamEncoderFromReified[Response]

  override def run(args: List[String]): IO[ExitCode] = {
    Blocker[IO].use { bl =>
      tcp.SocketGroup[IO](bl).use { sg =>
        serve(1234, sg).compile.drain.as(ExitCode.Success)
      }
    }
  }

  def serve(port: Int, sg: tcp.SocketGroup): Stream[IO, Unit] =
    serveAddr(port, sg).as(())

  def serveAddr(port: Int, sg: tcp.SocketGroup): Stream[IO, InetSocketAddress] = {
    Stream.resource(sg.serverResource[IO](addr(port))).flatMap {
      case (localAddr, sockets) =>
        val x = sockets.flatMap { socket =>
          Stream.resource(socket).map { socket =>
            val bvs: Stream[IO, BitVector] = socket.reads(bufferSize, timeout).chunks.map(ch => BitVector.view(ch.toArray))
            val requests: Stream[IO, Request] = bvs.through(decPipe[IO, Request])
            val responses: Stream[IO, Response] = requests.flatMap(req => Stream.eval(logic(req)))
            val encoded: Stream[IO, Byte] = resCodec.encode(responses).flatMap { bv =>
              Stream.chunk(Chunk.bytes(bv.bytes.toArray))
            }

            encoded.through(socket.writes(timeout)).onFinalize(socket.endOfOutput)
          }
        }

        Stream.emit(localAddr) ++ x.parJoin(maxClients).drain
    }
  }

  def logic(req: Request): IO[Response] = req match {
    case Random(min, max) =>
      if (min < max) {
        IO {
          val v = rnd.nextInt(max - min + 1) + min
          RandInt(v)
        }
      } else if (min === max) {
        IO.pure(RandInt(min))
      } else {
        IO.raiseError(new IllegalArgumentException("min must not be greater than max"))
      }
    case Seed(s) =>
      IO {
        rnd.setSeed(s)
        Seeded
      }
  }
}

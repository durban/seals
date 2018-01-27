/*
 * Copyright 2016-2018 Daniel Urban and contributors listed in AUTHORS
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
import java.nio.channels.{ AsynchronousChannelGroup => ACG }
import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

import cats.implicits._
import cats.effect.IO

import fs2.{ Stream, Chunk }
import fs2.io.tcp

import scodec.bits.BitVector
import scodec.stream.StreamCodec

import io.sigs.seals.scodec.StreamCodecs._
import io.sigs.seals.scodec.StreamCodecs.{ pipe => decPipe }

import Protocol.v1.{ Request, Response, Random, Seed, RandInt, Seeded }

object Server {

  final val bufferSize = 32 * 1024
  final val timeout = Some(2.seconds)
  final val maxClients = 200

  val rnd = new scala.util.Random

  def addr(port: Int): InetSocketAddress =
    new InetSocketAddress(InetAddress.getLoopbackAddress, port)

  val reqCodec: StreamCodec[Request] = StreamCodec[Request]
  val resCodec: StreamCodec[Response] = StreamCodec[Response]

  def main(args: Array[String]): Unit = {
    val ex = Executors.newCachedThreadPool()
    val cg = ACG.withThreadPool(ex)
    try {
      serve(1234)(cg, ExecutionContext.global).compile.drain.unsafeRunSync()
    } finally {
      cg.shutdown()
      ex.shutdown()
    }
  }

  def serve(port: Int)(implicit acg: ACG, st: ExecutionContext): Stream[IO, Unit] =
    serveAddr(port)(acg, st).collect { case Right(u) => u }

  def serveAddr(port: Int)(implicit acg: ACG, st: ExecutionContext): Stream[IO, Either[InetSocketAddress, Unit]] = {
    val v: Stream[IO, Stream[IO, Either[InetSocketAddress, Unit]]] = tcp.serverWithLocalAddress[IO](addr(port)).flatMap {
      case Left(localAddr) =>
        Stream.emit(Stream(Left(localAddr)) : Stream[IO, Either[InetSocketAddress, Unit]])
      case Right(sockets) =>
        Stream.emit(sockets.flatMap { socket =>
          val bvs: Stream[IO, BitVector] = socket.reads(bufferSize, timeout).chunks.map(ch => BitVector.view(ch.toArray))
          val requests: Stream[IO, Request] = bvs.through(decPipe[IO, Request])
          val responses: Stream[IO, Response] = requests.flatMap(req => Stream.eval(logic(req)))
          val encoded: Stream[IO, Byte] = resCodec.encode(responses).flatMap { bv =>
            Stream.chunk(Chunk.bytes(bv.bytes.toArray))
          }

          encoded.to(socket.writes(timeout)).map(Right(_)).onFinalize(socket.endOfOutput)
        })
    }

    v.join(maxClients)
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

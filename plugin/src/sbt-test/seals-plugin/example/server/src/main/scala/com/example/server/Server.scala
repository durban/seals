/*
 * Copyright 2017 Daniel Urban
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
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import cats.implicits._
import cats.effect.IO

import fs2.{ Stream, Chunk }
import fs2.io.tcp

import scodec.bits.BitVector
import scodec.Codec

import io.sigs.seals.scodec.Codecs._

import com.example.proto._

object Server {

  final val bufferSize = 32 * 1024
  final val timeout = Some(2.seconds)
  final val maxClients = 200
  final val port = 8080

  val rnd = new scala.util.Random

  def addr(port: Int): InetSocketAddress =
    new InetSocketAddress(InetAddress.getLoopbackAddress, port)

  def main(args: Array[String]): Unit = {
    val ex = Executors.newCachedThreadPool()
    implicit val cg = AsynchronousChannelGroup.withThreadPool(ex)
    try {
      serve(port).collect { case Right(_) => () }.run.unsafeRunSync()
    } finally {
      cg.shutdown()
      ex.shutdown()
    }
  }

  def serve(port: Int)(implicit acg: AsynchronousChannelGroup, ec: ExecutionContext): Stream[IO, Either[Int, Unit]] = {
    val s: Stream[IO, Stream[IO, Either[Int, Unit]]] = tcp.serverWithLocalAddress[IO](addr(port)).flatMap {
      case Left(localAddr) =>
        Stream.emit(Stream.emit(Left(localAddr.getPort) : Either[Int, Unit]).covary[IO]).covary[IO]
      case Right(sockets) =>
        Stream.emit(sockets.flatMap { socket =>
          val bvs: Stream[IO, BitVector] = socket.reads(bufferSize, timeout).chunks.map(ch => BitVector.view(ch.toArray))
          val tsk: IO[BitVector] = bvs.runLog.map(_.foldLeft(BitVector.empty)(_ ++ _))
          val request: IO[Request] = tsk.flatMap { bv =>
            Codec[Request].decode(bv).fold(
              err => IO.raiseError(new Exception(err.toString)),
              result => IO.pure(result.value)
            )
          }

          val response: IO[Response] = request.flatMap(logic)
          val encoded: Stream[IO, Byte] = Stream.eval(response)
            .map(r => Codec[Response].encode(r).require)
            .flatMap { bv =>
              Stream.chunk(Chunk.bytes(bv.bytes.toArray))
            }

          encoded.to(socket.writes(timeout)).onFinalize(socket.endOfOutput).map(Right(_))
        })
    }
    s.join(maxClients)
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

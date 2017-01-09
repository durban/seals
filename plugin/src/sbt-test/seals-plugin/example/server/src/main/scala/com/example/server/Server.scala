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

import cats.implicits._

import fs2.{ Stream, Task, Strategy, Chunk }
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
    val cg = AsynchronousChannelGroup.withThreadPool(ex)
    val st = Strategy.fromExecutor(ex)
    try {
      serve(port)(cg, st).run.unsafeRun()
    } finally {
      cg.shutdown()
      ex.shutdown()
    }
  }

  def serve(port: Int)(implicit acg: AsynchronousChannelGroup, st: Strategy): Stream[Task, Unit] = {
    val s: Stream[Task, Stream[Task, Unit]] = tcp.server[Task](addr(port)).flatMap { sockets =>
      Stream.emit(sockets.flatMap { socket =>
        val bvs: Stream[Task, BitVector] = socket.reads(bufferSize, timeout).chunks.map(ch => BitVector.view(ch.toArray))
        val tsk: Task[BitVector] = bvs.runLog.map(_.foldLeft(BitVector.empty)(_ ++ _))
        val request: Task[Request] = tsk.flatMap { bv =>
          Codec[Request].decode(bv).fold(
            err => Task.fail(new Exception(err.toString)),
            result => Task.now(result.value)
          )
        }

        val response: Task[Response] = request.flatMap(logic)
        val encoded: Stream[Task, Byte] = Stream.eval(response)
          .map(r => Codec[Response].encode(r).require)
          .mapChunks { ch =>
            Chunk.bytes(ch.foldLeft(BitVector.empty)(_ ++ _).bytes.toArray)
          }

        encoded.to(socket.writes(timeout)).onFinalize(socket.endOfOutput)
      })
    }
    fs2.concurrent.join(maxClients)(s)
  }

  def logic(req: Request): Task[Response] = req match {
    case RandomNumber(min, max) =>
      if (min < max) {
        Task.delay {
          val v = rnd.nextInt(max - min + 1) + min
          Number(v)
        }
      } else if (min === max) {
        Task.now(Number(min))
      } else {
        Task.fail(new IllegalArgumentException("min must not be greater than max"))
      }
    case ReSeed(s) =>
      Task.delay {
        rnd.setSeed(s)
        Ok
      }
  }
}

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

package com.example.lib

import java.net.{ InetSocketAddress, InetAddress }
import java.nio.channels.{ AsynchronousChannelGroup => ACG }
import java.util.concurrent.Executors

import scala.concurrent.duration._

import cats.implicits._

import fs2.{ Stream, Task, Strategy, Chunk }
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
  val addr = new InetSocketAddress(InetAddress.getLoopbackAddress, 1234)

  val reqCodec: StreamCodec[Request] = StreamCodec[Request]
  val resCodec: StreamCodec[Response] = StreamCodec[Response]

  def main(args: Array[String]): Unit = {
    val ex = Executors.newCachedThreadPool()
    val cg = ACG.withThreadPool(ex)
    val st = Strategy.fromExecutor(ex)
    try {
      serve(cg, st).run.unsafeRun()
    } finally {
      cg.shutdown()
      ex.shutdown()
    }
  }

  def serve(implicit acg: ACG, st: Strategy): Stream[Task, Unit] = {
    val s: Stream[Task, Stream[Task, Unit]] = tcp.server[Task](addr).flatMap { sockets =>
      Stream.emit(sockets.flatMap { socket =>
        val bvs: Stream[Task, BitVector] = socket.reads(bufferSize, timeout).chunks.map(ch => BitVector.view(ch.toArray))
        val requests: Stream[Task, Request] = bvs.through(decPipe[Task, Request])
        val responses: Stream[Task, Response] = requests.flatMap(req => Stream.eval(logic(req)))
        val encoded: Stream[Task, Byte] = resCodec.encode(responses).mapChunks { ch =>
          Chunk.bytes(ch.foldLeft(BitVector.empty)(_ ++ _).bytes.toArray)
        }

        encoded.to(socket.writes(timeout)).onFinalize(socket.endOfOutput)
      })
    }
    fs2.concurrent.join(maxClients)(s)
  }

  def logic(req: Request): Task[Response] = req match {
    case Random(min, max) =>
      if (min < max) {
        Task.delay {
          val v = rnd.nextInt(max - min + 1) + min
          RandInt(v)
        }
      } else if (min === max) {
        Task.now(RandInt(min))
      } else {
        Task.fail(new IllegalArgumentException("min must not be greater than max"))
      }
    case Seed(s) =>
      Task.delay {
        rnd.setSeed(s)
        Seeded
      }
  }
}

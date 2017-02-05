/*
 * Copyright 2016-2017 Daniel Urban
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

import java.nio.channels.{ AsynchronousChannelGroup => ACG }
import java.util.concurrent.Executors

import org.scalatest.{ BeforeAndAfterAll, FlatSpec, Matchers, Inside }

import fs2.{ Stream, Task, Strategy, Chunk }

import scodec.bits._
import scodec.stream.StreamCodec

import io.sigs.seals.scodec.StreamCodecs
import io.sigs.seals.scodec.StreamCodecs._

import Protocol.v2
import Protocol.v1.Seed

class IncompatibleClientSpec
    extends FlatSpec
    with Matchers
    with Inside
    with BeforeAndAfterAll {

  val ex = Executors.newCachedThreadPool()
  implicit val cg = ACG.withThreadPool(ex)
  implicit val st = Strategy.fromExecutor(ex)

  val reqCodec: StreamCodec[v2.Request] = StreamCodec[v2.Request]

  override def afterAll(): Unit = {
    super.afterAll()
    cg.shutdown()
    ex.shutdown()
  }

  "Client with incompatible schema" should "be rejected" in {
    val resp: Either[Throwable, Vector[v2.Response]] = fs2.concurrent.join(Int.MaxValue) {
      Server.serveAddr(0).map {
        case Left(localAddr) =>
          incompatibleClient(localAddr.getPort)
        case Right(_) =>
          Stream.empty[Task, v2.Response]
      }
    }.take(1L).runLog.unsafeAttemptRun()

    inside(resp) {
      case Left(ex) => ex.getMessage should include ("incompatible models")
    }
  }

  def incompatibleClient(port: Int): Stream[Task, v2.Response] = {
    fs2.io.tcp.client[Task](Server.addr(port)).flatMap { socket =>
      val bvs: Stream[Nothing, BitVector] = reqCodec.encode(Stream(Seed(42): v2.Request))
      val bs: Stream[Nothing, Byte] = bvs.mapChunks { ch =>
        Chunk.bytes(ch.foldLeft(BitVector.empty)(_ ++ _).bytes.toArray)
      }
      val read = bs.to(socket.writes(Server.timeout)).drain.onFinalize(socket.endOfOutput) ++
        socket.reads(Server.bufferSize, Server.timeout).chunks.map(ch => BitVector.view(ch.toArray))
      read.through(StreamCodecs.pipe[Task, v2.Response])
    }
  }
}

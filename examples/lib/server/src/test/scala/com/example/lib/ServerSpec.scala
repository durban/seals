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

import java.util.concurrent.Executors
import java.nio.channels.{ AsynchronousChannelGroup => ACG }

import scala.concurrent.ExecutionContext

import cats.effect.IO

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import fs2.{ Stream, Chunk }

import scodec.bits._

import Protocol.v1.{ Response, Request, Random, RandInt, Seed, Seeded }

import dev.tauri.seals.scodec.StreamCodecs

class ServerSpec extends AnyFlatSpec with Matchers with TcpTest {

  val ex = Executors.newCachedThreadPool()
  implicit val cg = ACG.withThreadPool(ex)
  implicit override lazy val ec = ExecutionContext.global

  override def afterAll(): Unit = {
    super.afterAll()
    cg.shutdown()
    ex.shutdown()
  }

  "Server" should "respond to requests" in {
    val nClients = 20
    val nRandom = testData.count {
      case Random(_, _) => true
      case _ => false
    }
    val nSeed = testData.count {
      case Seed(_) => true
      case _ => false
    }
    val str: Stream[IO, Stream[IO, Response]] = Server.serveAddr(0, this.sockGroup).map { localAddr =>
      clients(localAddr.getPort, nClients)
    }
    val responses: Vector[Response] = str
      .parJoin(Int.MaxValue)
      .take((nClients * testData.size).toLong).compile.toVector.unsafeRunSync()

    val randInts = responses.collect { case RandInt(i) => i }
    val seededs = responses.collect { case Seeded => () }

    randInts.size should === (nClients * nRandom)
    randInts.foreach { i =>
      i should be >= 1
      i should be <= 100
    }

    seededs.size should === (nClients * nSeed)
  }

  val testData = Vector[Request](
    Random(10, 19),
    Seed(0xdeadbeefL),
    Random(1, 100)
  )

  def clients(port: Int, count: Int, maxConcurrent: Int = 10): Stream[IO, Response] = {
    val cls: Stream[IO, Stream[IO, Response]] = {
      Stream.range(0, count).map { i =>
        Stream.resource(this.sockGroup.client[IO](Server.addr(port))).flatMap { socket =>
          val bvs: Stream[IO, BitVector] = StreamCodecs.streamEncoderFromReified[Request].encode(Stream.emits(testData).covary[IO])
          val bs: Stream[IO, Byte] = bvs.flatMap { bv =>
            Stream.chunk(Chunk.bytes(bv.bytes.toArray))
          }
          val read = bs.through(socket.writes(Server.timeout)).drain.onFinalize(socket.endOfOutput) ++
            socket.reads(Server.bufferSize, Server.timeout).chunks.map(ch => BitVector.view(ch.toArray))
          read.through(StreamCodecs.pipe[IO, Response])
        }
      }
    }

    cls.parJoin(maxConcurrent)
  }
}

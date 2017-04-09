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

import java.util.concurrent.Executors
import java.nio.channels.{ AsynchronousChannelGroup => ACG }

import org.scalatest.{ FlatSpec, Matchers, BeforeAndAfterAll }

import fs2.{ Task, Stream, Chunk, Strategy }

import scodec.bits._

import Protocol.v1.{ Response, Request, Random, RandInt, Seed, Seeded }

import io.sigs.seals.scodec.StreamCodecs

class ServerSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  val ex = Executors.newCachedThreadPool()
  implicit val cg = ACG.withThreadPool(ex)
  implicit val st = Strategy.fromExecutor(ex)

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
    val responses: Vector[Response] = fs2.concurrent.join(Int.MaxValue)(
      Server.serveAddr(0).map {
        case Left(localAddr) =>
          clients(localAddr.getPort, nClients)
        case Right(_) =>
          Stream.empty
      }
    ).take((nClients * testData.size).toLong).runLog.unsafeRun

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

  def clients(port: Int, count: Int, maxConcurrent: Int = 10): Stream[Task, Response] = {
    val cls: Stream[Task, Stream[Task, Response]] = {
      Stream.range(0, count).map { i =>
        fs2.io.tcp.client[Task](Server.addr(port)).flatMap { socket =>
          val bvs: Stream[Nothing, BitVector] = Server.reqCodec.encode(Stream(testData: _*))
          val bs: Stream[Nothing, Byte] = bvs.mapChunks { ch =>
            Chunk.bytes(ch.foldLeft(BitVector.empty)(_ ++ _).bytes.toArray)
          }
          val read = bs.to(socket.writes(Server.timeout)).drain.onFinalize(socket.endOfOutput) ++
            socket.reads(Server.bufferSize, Server.timeout).chunks.map(ch => BitVector.view(ch.toArray))
          read.through(StreamCodecs.pipe[Task, Response])
        }
      }
    }

    fs2.concurrent.join(maxConcurrent)(cls)
  }
}

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

import java.util.concurrent.Executors
import java.nio.channels.{ AsynchronousChannelGroup => ACG }

import scala.concurrent.ExecutionContext.Implicits.global

import cats.effect.IO

import org.scalatest.{ FlatSpec, Matchers, BeforeAndAfterAll }

import fs2.{ Stream, Chunk }

import scodec.bits._
import scodec.Codec

import io.sigs.seals.scodec.Codecs._

import com.example.proto._

class ServerSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  val ex = Executors.newCachedThreadPool()
  implicit val cg = ACG.withThreadPool(ex)

  override def afterAll(): Unit = {
    super.afterAll()
    cg.shutdown()
    ex.shutdown()
  }

  "Server" should "respond to a request" in {
    val responses: Vector[Response] = Server.serve(0).map[Stream[IO, Response]] {
      case Left(port) =>
        client(port)
      case Right(_) =>
        Stream.empty
    }.join(Int.MaxValue).take(1L).runLog.unsafeRunSync()
    responses should === (Vector(Ok))
  }

  def client(port: Int): Stream[IO, Response] = {
    fs2.io.tcp.client[IO](Server.addr(port)).flatMap { socket =>
      val bvs: Stream[Nothing, BitVector] = Stream(Codec[Request].encode(ReSeed(56)).require)
      val bs: Stream[Nothing, Byte] = bvs.flatMap { bv =>
        Stream.chunk(Chunk.bytes(bv.bytes.toArray))
      }
      val read = bs.to(socket.writes(Server.timeout)).drain.onFinalize(socket.endOfOutput) ++
        socket.reads(Server.bufferSize, Server.timeout).chunks.map(ch => BitVector.view(ch.toArray))
      read.fold(BitVector.empty)(_ ++ _).map(bv => Codec[Response].decode(bv).require.value)
    }
  }
}

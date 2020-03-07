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

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

import cats.effect.{ IO, Blocker, ContextShift }

import org.scalatest.{ FlatSpec, Matchers, BeforeAndAfterAll }

import fs2.{ Stream, Chunk }

import scodec.bits._
import scodec.Codec

import io.sigs.seals.scodec.Codecs._

import com.example.proto._

class ServerSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val ex = Executors.newCachedThreadPool()
  val ec = ExecutionContext.fromExecutor(ex)
  val bl = Blocker.liftExecutionContext(ec)
  val (sg, closeSg) = fs2.io.tcp.SocketGroup[IO](bl).allocated.unsafeRunSync()

  override def afterAll(): Unit = {
    super.afterAll()
    closeSg.unsafeRunSync()
    ex.shutdown()
  }

  "Server" should "respond to a request" in {
    val responses: Vector[Response] = Stream(
      Server.serve(Server.port, sg).drain,
      client(Server.port)
    ).parJoin(Int.MaxValue).take(1L).compile.toVector.unsafeRunSync()
    responses should === (Vector(Ok))
  }

  def client(port: Int): Stream[IO, Response] = {
    Stream.resource(sg.client[IO](Server.addr(port))).flatMap { socket =>
      val bvs: Stream[IO, BitVector] = Stream(Codec[Request].encode(ReSeed(56)).require)
      val bs: Stream[IO, Byte] = bvs.flatMap { bv =>
        Stream.chunk(Chunk.bytes(bv.bytes.toArray))
      }
      val read = bs.through(socket.writes(Server.timeout)).drain.onFinalize(socket.endOfOutput) ++
        socket.reads(Server.bufferSize, Server.timeout).chunks.map(ch => BitVector.view(ch.toArray))
      read.fold(BitVector.empty)(_ ++ _).map(bv => Codec[Response].decode(bv).require.value)
    }
  }
}

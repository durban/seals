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

import cats.effect.IO

import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import fs2.{ Stream, Chunk }

import scodec.bits._
import scodec.stream.StreamEncoder

import dev.tauri.seals.scodec.StreamCodecs
import dev.tauri.seals.scodec.StreamCodecs._

import Protocol.v2
import Protocol.v1.Seed

class IncompatibleClientSpec
    extends AnyFlatSpec
    with Matchers
    with Inside
    with TcpTest {

  protected override def ec = scala.concurrent.ExecutionContext.global

  val reqCodec: StreamEncoder[v2.Request] = streamEncoderFromReified[v2.Request]

  override def afterAll(): Unit = {
    super.afterAll()
  }

  "Client with incompatible schema" should "be rejected" ignore { // TODO
    val resp: Either[Throwable, Vector[v2.Response]] = Server.serveAddr(0, sockGroup).flatMap { localAddr =>
      incompatibleClient(localAddr.getPort)
    }.take(1L).compile.toVector.attempt.unsafeRunSync()

    inside(resp) {
      case Left(ex) => ex.getMessage should include ("incompatible models")
    }
  }

  def incompatibleClient(port: Int): Stream[IO, v2.Response] = {
    Stream.resource(sockGroup.client[IO](Server.addr(port))).flatMap { socket =>
      val bvs: Stream[IO, BitVector] = reqCodec.encode[IO](Stream(Seed(42): v2.Request))
      val bs: Stream[IO, Byte] = bvs.flatMap { bv =>
        Stream.chunk(Chunk.bytes(bv.bytes.toArray))
      }
      val read = bs.through(socket.writes(Server.timeout)).drain.onFinalize(socket.endOfOutput) ++
        socket.reads(Server.bufferSize, Server.timeout).chunks.map(ch => BitVector.view(ch.toArray))
      read.through(StreamCodecs.pipe[IO, v2.Response])
    }
  }
}

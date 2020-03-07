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

import scala.concurrent.Await
import scala.concurrent.duration._

import cats.effect.IO

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

import fs2.Stream

import akka.actor.ActorSystem
import akka.stream.{ Materializer, ActorMaterializer }

import Protocol.v1.{ Response, RandInt, Seeded }

class ClientSpec extends AnyFlatSpec with Matchers with com.example.lib.TcpTest {

  implicit lazy val sys: ActorSystem = ActorSystem("InteropSpec")
  implicit lazy val mat: Materializer = ActorMaterializer()

  protected override def ec = sys.dispatcher

  override def afterAll(): Unit = {
    super.afterAll()
    sys.terminate()
  }

  "Client" should "receive the correct response" in {
    val sem = cats.effect.concurrent.Semaphore[IO](0).unsafeRunSync()
    Stream(Server.serve(1237, sockGroup).drain, Stream.eval(sem.acquire))
      .parJoin(Int.MaxValue)
      .take(1)
      .compile
      .drain
      .unsafeRunAsync(_ => ())
    try {
      val resp = Await.result(Client.client(1237), 2.seconds)
      // constant, because we always seed with the same value:
      resp should === (Vector[Response](Seeded, RandInt(42)))
    } finally {
      sem.release.unsafeRunSync()
    }
  }
}

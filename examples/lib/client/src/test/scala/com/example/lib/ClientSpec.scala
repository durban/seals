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

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._

import cats.effect.IO

import org.scalatest.{ FlatSpec, Matchers, BeforeAndAfterAll }

import fs2.Stream

import akka.actor.ActorSystem
import akka.stream.{ Materializer, ActorMaterializer }

import Protocol.v1.{ Response, RandInt, Seeded }

class ClientSpec extends FlatSpec with Matchers with BeforeAndAfterAll {

  implicit val sys: ActorSystem = ActorSystem("InteropSpec")
  implicit val mat: Materializer = ActorMaterializer()
  implicit val ec: ExecutionContext = sys.dispatcher
  implicit val cg = ACG.withThreadPool(Executors.newCachedThreadPool())

  override def afterAll(): Unit = {
    super.afterAll()
    sys.terminate()
    cg.shutdown()
  }

  "Client" should "receive the correct response" in {
    val sem = fs2.async.mutable.Semaphore.empty[IO].unsafeRunSync()
    Stream(Server.serve(1237).drain, Stream.eval(sem.decrement))
      .join(Int.MaxValue)
      .take(1)
      .runLog
      .unsafeRunAsync(_ => ())
    try {
      val resp = Await.result(Client.client(1237), 2.seconds)
      // constant, because we always seed with the same value:
      resp should === (Vector[Response](Seeded, RandInt(42)))
    } finally {
      sem.increment.unsafeRunSync()
    }
  }
}

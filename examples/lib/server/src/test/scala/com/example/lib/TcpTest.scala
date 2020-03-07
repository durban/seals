/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.effect.{ ContextShift, IO, Blocker }

import org.scalatest.{ Suite, BeforeAndAfterAll }
import scala.concurrent.ExecutionContext

trait TcpTest extends BeforeAndAfterAll { this: Suite =>

  protected def ec: ExecutionContext

  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  val (blocker, closeBlocker) = Blocker.fromExecutorService[IO](IO { Executors.newFixedThreadPool(8) }).allocated.unsafeRunSync()
  val (sockGroup, closeSockGroup) = fs2.io.tcp.SocketGroup[IO](blocker).allocated.unsafeRunSync()

  override def afterAll(): Unit = {
    super.afterAll()
    closeSockGroup.unsafeRunSync()
    closeBlocker.unsafeRunSync()
  }
}

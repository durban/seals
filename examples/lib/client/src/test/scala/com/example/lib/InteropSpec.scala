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

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._

import org.scalatest.{ FlatSpec, Matchers }

import akka.actor.ActorSystem
import akka.stream.{ Materializer, ActorMaterializer }
import akka.stream.scaladsl.{ Source, Keep }

import scodec.bits._

class InteropSpec extends FlatSpec with Matchers {

  implicit val sys: ActorSystem = ActorSystem("InteropSpec")
  implicit val mat: Materializer = ActorMaterializer()
  implicit val ec: ExecutionContext = sys.dispatcher

  val pat: BitVector = hex"deadbeef".bits
  val count = 10
  val src: Source[BitVector, _] = Source.tick(
    1.second,
    1.second,
    pat
  ).take(count.toLong)
  val exp: BitVector = Vector.fill(count)(pat).fold(BitVector.empty)(_ ++ _)

  "toLazyBitVector" should "work" in {
    val bv: BitVector = Interop.toLazyBitVector(src, bufferSize = 100).force
    bv should === (exp)
  }

  "lazyBitVectorSink" should "work" in {
    val sink = Interop.lazyBitVectorSink(bufferSize = 100)
    val fut = src.toMat(sink)(Keep.right).run()
    val bv = Await.result(fut, 15.seconds)
    bv should === (exp)
  }
}

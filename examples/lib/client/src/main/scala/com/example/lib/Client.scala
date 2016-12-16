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

import java.net.{ InetAddress, InetSocketAddress }

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.util.{ ByteString }

import scala.concurrent.Await

import scodec.Codec
import scodec.bits.BitVector
import scodec.interop.akka._
import scodec.stream.codec.StreamCodec

import io.sigs.seals._
import io.sigs.seals.scodec.Codecs._
import io.sigs.seals.scodec.StreamCodecs._

import Protocol.v1.{ Request, Response, Seed, Random }

object Client {

  val addr = new InetSocketAddress(InetAddress.getLoopbackAddress, 1234)

  val reqCodec: Codec[Request] = Codec[Request]
  val resCodec: StreamCodec[Response] = StreamCodec[Response]

  def main(args: Array[String]): Unit = {
    implicit val sys: ActorSystem = ActorSystem("ClientSystem")
    implicit val mat: Materializer = ActorMaterializer()
    try {
      val resp = Await.result(client, 10.seconds)
      println(resp)
    } finally {
      sys.terminate()
    }
  }

  def client(implicit sys: ActorSystem, mat: Materializer): Future[Vector[Response]] = {
    import sys.dispatcher
    val graph = Tcp().outgoingConnection(addr).joinMat(logic)(Keep.right)
    graph.run().flatMap { bv =>
      Interop.futureFromTask(resCodec.decode(bv).runLog)
    }
  }

  def logic(implicit sys: ActorSystem): Flow[ByteString, ByteString, Future[BitVector]] = {
    val requests = Source[Request](
      Seed(0xabcdL) :: Random(1, 100) :: Nil
    ).map(reqCodec.encode)
    val model = Source.single(
      Reified[Request].model
    ).map(Codec[Model].encode)
    val source = model.concat(requests)
      .map(_.getOrElse(throw new Exception("impossible")))
      .map(_.bytes.toByteString)
    val sink = Interop.lazyBitVectorSink()(sys.dispatcher)
      .contramap[ByteString](_.toByteVector.bits)
    Flow.fromSinkAndSourceMat(sink, source)(Keep.left)
  }
}

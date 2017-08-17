/*
 * Copyright 2016 Daniel Urban and contributors listed in AUTHORS
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
import scala.concurrent.Await
import scala.concurrent.duration._

import cats.effect.IO

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.util.{ ByteString }

import scodec.interop.akka._
import scodec.stream.codec.StreamCodec

import streamz.converter._

import io.sigs.seals.scodec.StreamCodecs._
import io.sigs.seals.scodec.StreamCodecs.{ pipe => decPipe }

import Protocol.v1.{ Request, Response, Seed, Random }

object Client {

  val reqCodec: StreamCodec[Request] = StreamCodec[Request]
  val resCodec: StreamCodec[Response] = StreamCodec[Response]

  def main(args: Array[String]): Unit = {
    implicit val sys: ActorSystem = ActorSystem("ClientSystem")
    implicit val mat: Materializer = ActorMaterializer()
    try {
      val resp = Await.result(client(1234), 10.seconds)
      println(resp)
    } finally {
      sys.terminate()
    }
  }

  def client(port: Int)(implicit sys: ActorSystem, mat: Materializer): Future[Vector[Response]] = {
    val addr = new InetSocketAddress(InetAddress.getLoopbackAddress, port)
    Tcp().outgoingConnection(addr).joinMat(logic)(Keep.right).run()
  }

  def logic(implicit sys: ActorSystem, mat: Materializer): Flow[ByteString, ByteString, Future[Vector[Response]]] = {
    import sys.dispatcher

    val requests = fs2.Stream(Seed(0xabcdL), Random(1, 100))
    val source = Source.fromGraph(reqCodec.encode(requests).toSource)
      .map(_.toByteVector.toByteString)

    val decode: Flow[ByteString, Response, NotUsed] = Flow.fromGraph(Flow[ByteString]
      .map(_.toByteVector.bits)
      .toPipe(_ => ())
      .andThen(decPipe[IO, Response])
      .toFlow
    )
    val sink: Sink[ByteString, Future[Vector[Response]]] = decode.toMat(
      Sink.fold(Vector.empty[Response])(_ :+ _)
    )(Keep.right)

    Flow.fromSinkAndSourceMat(sink, source)(Keep.left)
  }
}

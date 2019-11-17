/*
 * Copyright 2016-2019 Daniel Urban and contributors listed in AUTHORS
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

import scodec.bits.BitVector
import scodec.stream.codec.StreamCodec

import fs2.interop.reactivestreams._

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

  def logic(implicit sys: ActorSystem): Flow[ByteString, ByteString, Future[Vector[Response]]] = {
    import sys.dispatcher

    val requests = fs2.Stream(Seed(0xabcdL), Random(1, 100)).covary[IO]
    val source = Source
      .fromPublisher(reqCodec.encode(requests).toUnicastPublisher())
      .map(bv => ByteString.fromArrayUnsafe(bv.toByteArray))

    // TODO: this would be much less ugly, if we had a decoder `Flow`
    val buffer = fs2.async.unboundedQueue[IO, Option[BitVector]].unsafeRunSync()
    val decode: Flow[ByteString, Response, NotUsed] = Flow.fromSinkAndSource(
      Sink.onComplete { _ =>
        buffer.enqueue1(None).unsafeRunSync()
      }.contramap[ByteString] { x =>
        buffer.enqueue1(Some(BitVector.view(x.toArray))).unsafeRunSync()
      },
      Source.fromPublisher(buffer
        .dequeue
        .unNoneTerminate
        .through(decPipe[IO, Response])
        .toUnicastPublisher()
      )
    )
    val sink: Sink[ByteString, Future[Vector[Response]]] = decode.toMat(
      Sink.fold(Vector.empty[Response])(_ :+ _)
    )(Keep.right)

    Flow.fromSinkAndSourceMat(sink, source)(Keep.left)
  }
}

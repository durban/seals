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

import scala.concurrent.{ Future, Promise, ExecutionContext }

import cats.implicits._

import akka.stream.Materializer
import akka.stream.scaladsl.{ Flow, Source, Sink, Keep }

import scodec.bits.BitVector

import fs2.{ Task, Strategy }
import fs2.async.mutable.Queue

/** This is a hack to bridge some things between FS2/scodec and Akka streams */
object Interop {

  def futureFromTask[A](tsk: Task[A])(implicit ec: ExecutionContext): Future[A] = {
    implicit val str: Strategy = Strategy.fromExecutionContext(ec)
    val p = Promise[A]()
    tsk.unsafeRunAsync {
      case Left(ex) => p.failure(ex)
      case Right(a) => p.success(a)
    }
    p.future
  }

  def lazyBitVectorSink(bufferSize: Int = 100)(implicit ec: ExecutionContext): Sink[BitVector, Future[BitVector]] = {
    implicit val str: Strategy = Strategy.fromExecutionContext(ec)
    val flow = Flow[BitVector]
      .map(Some(_))
      .concat(Source.single(None))
    val sink = Sink.fold[(Task[Unit], Queue[Task, Option[BitVector]]), Option[BitVector]] (
      (Task.now(()), Queue.bounded[Task, Option[BitVector]](bufferSize).unsafeRun)
    ) { (st, opt) => st match {
      case (tsk, queue) => (tsk.flatMap(_ => queue.enqueue1(opt)), queue)
    }}

    flow.toMat(sink)(Keep.right).mapMaterializedValue { fut =>
      fut.map {
        case (fill, queue) =>
          fill.async.unsafeRunAsync(_ => ())
          BitVector.unfold(()) { _ => queue.dequeue1.unsafeRun.map { b => (b, ()) } }
      }
    }
  }

  def toLazyBitVector(in: Source[BitVector, _], bufferSize: Int = 100)(implicit mat: Materializer): BitVector = {
    implicit val str: fs2.Strategy = fs2.Strategy.fromExecutor(mat.executionContext)
    val queue = fs2.async.mutable.Queue.bounded[Task, Option[BitVector]](bufferSize).unsafeRun

    val fill: Task[Unit] = Task.async[Task[Unit]] { cb =>
      val fut = in.map(Some(_)).concat(Source.single(None)).runFold[Task[Unit]](Task.now(())) {
        (tsk, opt) => tsk.flatMap(_ => queue.enqueue1(opt))
      }
      fut.onComplete { t => cb(Either.fromTry(t)) } (mat.executionContext)
    }.flatMap(x => x)
    fill.async.unsafeRunAsync(_ => ())

    BitVector.unfold(()) { _ => queue.dequeue1.unsafeRun.map { b => (b, ()) } }
  }
}

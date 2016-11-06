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

package com.example.streaming

import java.io.{ ByteArrayInputStream, ByteArrayOutputStream }

import shapeless.record._

import org.scalatest.{ FlatSpec, Matchers }

import fs2.{ Stream, Pure, Task }

import scodec.Codec
import scodec.bits.BitVector
import scodec.stream.decode.DecodingError

import io.sigs.seals._
import io.sigs.seals.scodec.Codecs._

class StreamingSpec extends FlatSpec with Matchers {

  import Main.{ Animal, Elephant, Quokka, Quagga, Grey }

  val animals = Vector[Animal](
    Elephant("Dumbo", tuskLength = 35.0f),
    Quokka("Nellie"),
    Quagga("Ford", speed = 120.0)
  )

  val transformedAnimals = Vector[Animal](
    Elephant("Dumbo", tuskLength = 35.0f + 17.0f),
    Quokka("Nellie", Grey)
  )

  val animalStream = Stream.emits[Task, Animal](animals)

  "Encoding/decoding" should "work correctly" in {
    val tsk: Task[Unit] = for {
      bv <- Main.encoder.encode(animalStream).runFold(BitVector.empty)(_ ++ _)
      as <- Main.decoder.decode(bv).runLog
    } yield {
      as should === (animals)
    }
    tsk.unsafeRun()
  }

  it should "fail with incompatible models" in {
    val mod = Reified[Record.`'Elephant -> Elephant, 'Quokka -> Quokka`.T].model
    val bv: BitVector = Codec[Model].encode(mod).getOrElse(fail)
    val tsk: Task[Unit] = for {
      as <- Main.decoder.decode(bv).runLog
    } yield {
      as should === (Vector.empty)
    }

    val ex = intercept[DecodingError] {
      tsk.unsafeRun()
    }
    ex.err.message should include ("incompatible models")
  }

  "Transformation" should "work correctly" in {
    val tsk: Task[Unit] = for {
      ibv <- Main.encoder.encode(animalStream).runFold(BitVector.empty)(_ ++ _)
      is = new ByteArrayInputStream(ibv.toByteArray)
      os = new ByteArrayOutputStream
      _ <- Main.transform(is, os)(Main.transformer)
      obv = BitVector(os.toByteArray())
      transformed <- Main.decoder.decode(obv).runFold(Vector.empty[Animal])(_ :+ _)
    } yield {
      transformed should === (transformedAnimals)
    }
    tsk.unsafeRun()
  }
}

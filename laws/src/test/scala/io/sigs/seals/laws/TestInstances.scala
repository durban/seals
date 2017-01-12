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

package io.sigs.seals
package laws

import java.util.UUID

import cats.~>
import cats.implicits._

import TestTypes.Whatever

object TestInstances {

  object atomic {

    implicit val atomicMyUUID: Atomic[MyUUID] =
      AtomicMyUUID

    private[this] final case object AtomicMyUUID
        extends Atomic[MyUUID]
        with Atomic.FallbackBinary[MyUUID] {

      def description: String =
        "MyUUID"

      def fromString(s: String): Either[String, MyUUID] = {
        try {
          Either.right(MyUUID(UUID.fromString(s)))
        } catch {
          case ex: IllegalArgumentException => Either.left(ex.getMessage)
        }
      }

      def stringRepr(a: MyUUID): String =
        a.uuid.toString

      val uuid: UUID =
        UUID.fromString("85a168db-6ce3-47e7-b8aa-e45aa075d523")
    }

    implicit val atomicWhatever: Atomic[Whatever.type] =
      AtomicWhatever

    private[this] object AtomicWhatever
        extends Atomic[Whatever.type]
        with Atomic.FallbackBinary[Whatever.type] {

      def description: String =
        "whatever"

      def fromString(s: String): Either[String, Whatever.type] = {
        if (Whatever.toString.equals(s)) Either.right(Whatever)
        else Either.left("not Whatever")
      }

      def stringRepr(a: Whatever.type): String =
        Whatever.toString

      val uuid: UUID =
        UUID.fromString("fa786e05-9baf-4a87-a06d-712d83d3c5d7")
    }

    object bad {

      implicit val atomicInt: Atomic[Int] =
        BadAtomicInt

      private[this] final object BadAtomicInt
          extends Atomic[Int]
          with Atomic.FallbackBinary[Int] {

        def description: String =
          "MyInt"

        def fromString(s: String): Either[String, Int] =
          Either.catchNonFatal(s.toInt).leftMap(_.getMessage)

        def stringRepr(a: Int): String =
          a.toString

        val uuid: UUID =
          UUID.fromString("fa786e05-9baf-4a87-a06d-712d83d3c5d7")
      }
    }
  }

  object kleene {

    implicit val kleeneForScalaStream: Kleene[Stream] = Kleene.instance(
      λ[Stream ~> Vector](_.toVector),
      λ[Vector ~> Stream](_.toStream)
    )
  }

  object reified {

    implicit val imapReifiedForUuid: Reified[UUID] = {
      Reified[(Long, Long)].imap[UUID] { case (a, b) =>
        new UUID(a, b)
      } { uuid =>
        (uuid.getMostSignificantBits, uuid.getLeastSignificantBits)
      }
    }
  }
}

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

import scala.util.Try

import cats.data.Xor

import TestTypes.Whatever

object TestInstances {

  object atomic {

    implicit val atomicUUID: Atomic[UUID] =
      AtomicUUID

    private[this] final case object AtomicUUID extends Atomic[UUID] {

      def description: String =
        "java.util.UUID"

      def fromString(s: String): Xor[String, UUID] = {
        try {
          Xor.right(UUID.fromString(s))
        } catch {
          case ex: IllegalArgumentException => Xor.left(ex.getMessage)
        }
      }

      def stringRepr(a: UUID): String =
        a.toString

      val uuid: UUID =
        UUID.fromString("85a168db-6ce3-47e7-b8aa-e45aa075d523")
    }

    implicit val atomicWhatever: Atomic[Whatever.type] = new Atomic[Whatever.type] {

      def description: String =
        "whatever"

      def fromString(s: String): Xor[String, Whatever.type] =
        Xor.right(Whatever)

      def stringRepr(a: Whatever.type): String =
        Whatever.toString

      val uuid: UUID =
        UUID.fromString("fa786e05-9baf-4a87-a06d-712d83d3c5d7")
    }

    object bad {

      implicit val atomicInt: Atomic[Int] = new Atomic[Int] {

        def description: String =
          "MyInt"

        def fromString(s: String): Xor[String, Int] =
          Xor.catchNonFatal(s.toInt).leftMap(_.getMessage)

        def stringRepr(a: Int): String =
          a.toString

        val uuid: UUID =
          UUID.fromString("fa786e05-9baf-4a87-a06d-712d83d3c5d7")
      }
    }
  }
}

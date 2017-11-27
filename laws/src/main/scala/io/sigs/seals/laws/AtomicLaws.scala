/*
 * Copyright 2016-2017 Daniel Urban and contributors listed in AUTHORS
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
import java.time.DayOfWeek

import cats.Eq
import cats.kernel.laws._
import cats.kernel.laws.discipline._
import cats.implicits._

import scodec.bits._
import scodec.interop.cats._

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

import ArbInstances.arbByteVector

object AtomicLaws {

  def apply[A](implicit arb: Arbitrary[A], atc: Atomic[A], equ: Eq[A]): AtomicLaws[A] = new AtomicLaws[A] {
    def Arb = arb
    def Atc = atc
    def Equ = equ
  }

  object DerivedAtomicTester
      extends Atomic.Derived[Byte, Int] {

    def from(b: Int): Either[Atomic.Error, Byte] = {
      if ((b >= Byte.MinValue) && (b <= Byte.MaxValue)) Right(b.toByte)
      else Left(Atomic.Error(sh"out of range: ${b}"))
    }

    def to(a: Byte): Int =
      a.toInt

    def description: String =
      "DerivedByteFromInt"

    def uuid: UUID =
      uuid"b1452721-fe47-4649-aa8b-55205b8e1098"
  }

  object FallbackStringTester
      extends Atomic[Int]
      with Atomic.FallbackString[Int] {

    override def binaryRepr(a: Int): ByteVector =
      ByteVector.fromInt(a, 4, ByteOrdering.LittleEndian)

    override def fromBinary(b: ByteVector): Either[Atomic.Error, (Int, ByteVector)] = {
      if (b.length < 4L) {
        Left(Atomic.InsufficientData(4L, b.length))
      } else {
        val (d, r) = b.splitAt(4L)
        Right((d.toInt(signed = true, ByteOrdering.LittleEndian), r))
      }
    }

    override def description: String =
      "FallbackStringTester"

    override val uuid: UUID =
      uuid"cc03cb40-7c79-405d-add3-5bb51542b954"
  }

  object FallbackBinaryTester
      extends Atomic[Int]
      with Atomic.FallbackBinary[Int] {

    override def stringRepr(a: Int): String =
      a.toString

    override def fromString(s: String): Either[Atomic.Error, Int] = {
      try { Right(s.toInt) }
      catch { case ex: NumberFormatException => Left(Atomic.Error(ex.getMessage)) }
    }

    override def description: String =
      "FallbackBinaryTester"

    override val uuid: UUID =
      uuid"a848f0f6-9586-427f-aa17-d5c34c9cbbb6"
  }

  object ForEnumTester
      extends Atomic.ForEnum[DayOfWeek] {

    override def description: String =
      "ForEnumTester"

    override val uuid: UUID =
      uuid"090c78d0-86a4-4b32-9556-26b3a124354a"
  }
}

trait AtomicLaws[A] extends Laws {

  implicit def Arb: Arbitrary[A]
  implicit def Atc: Atomic[A]
  implicit def Equ: Eq[A]

  def roundtrip: this.RuleSet = new AtomicRuleSet(
    name = "roundtrip",
    "stringRepr-fromString" -> forAll { (a: A) =>
      Atc.fromString(Atc.stringRepr(a)) <-> Either.right(a)
    },
    "fromString-stringRepr" -> forAll { (s: String) =>
      Atc.fromString(s).fold(
        err => provedIsEq[String],
        a => {
          Atc.stringRepr(a) <-> s
        }
      )
    },
    "binaryRepr-fromBinary" -> forAll { (a: A) =>
      Atc.fromBinary(Atc.binaryRepr(a)) <-> Either.right((a, ByteVector.empty))
    },
    "fromBinary-binaryRepr" -> forAll { (b: ByteVector) =>
      Atc.fromBinary(b).fold(
        err => provedIsEq[ByteVector],
        { case (a, r) => (Atc.binaryRepr(a) ++ r) <-> b }
      )
    }
  )

  final class AtomicRuleSet(
    val name: String,
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val parent = None
    val bases = Nil
  }
}

/*
 * Copyright 2016-2017 Daniel Urban
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
package tests

import java.util.UUID
import java.math.{ MathContext, RoundingMode }

import org.scalatest.Inside
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import cats.implicits._

import scodec.bits._

import laws.MyUUID
import laws.TestArbInstances.arbUuid
import laws.TestInstances.atomic._
import laws.TestTypes.{ CaseClass, Whatever }

class AtomicSpec extends BaseSpec with GeneratorDrivenPropertyChecks with Inside {

  val atomic = Atomic[MyUUID]
  val whatever = Atomic[Whatever.type]

  "Automatic Model derivation" in {
    atomic.description should === ("MyUUID")
    atomic.uuid should === (UUID.fromString("85a168db-6ce3-47e7-b8aa-e45aa075d523"))
  }

  "equals + hashCode" in {
    checkEqHash(atomic, atomic)
    checkNotEqHash(atomic, whatever)
    checkNotEqHash(atomic, Atomic[String])
  }

  "Serializable" in {
    checkSer(atomic)
    checkSer(whatever)
  }

  "stringRepr/fromString and binaryRepr/fromBinary" in {
    forAll { u: MyUUID =>
      roundtripStr(u)(atomic) should === (u)
      roundtripBin(u)(atomic) should === (u)
    }
  }

  "custom Atomic has higher priority than built-in" in {
    import laws.TestInstances.atomic.bad._
    Model.Atom.atom[Int].uuid should === (laws.TestInstances.atomic.bad.atomicInt.uuid)
  }

  "have higher priority than generic Reified" in {
    // derived instance:
    val r1 = Reified[CaseClass]

    // define an atomic:
    val constUuid = UUID.fromString("3ba1f17e-c0cd-4b17-8cca-771e90b60498")
    val r2 = {
      object ACC extends Atomic[CaseClass] with Atomic.FallbackBinary[CaseClass] {

        def description: String = "CaseClass"

        def fromString(s: String): Either[Atomic.Error, CaseClass] = {
          try {
            Either.right(CaseClass(s.toLong))
          } catch {
            case ex: IllegalArgumentException => Either.left(Atomic.Error(ex.getMessage))
          }
        }

        def stringRepr(a: CaseClass): String = a.n.toString

        val uuid: UUID = constUuid
      }

      implicit val atomicCaseClass: Atomic[CaseClass] = ACC

      // now this should take priority over the derived instance:
      Reified[CaseClass]
    }

    r1.model should !== (r2.model)

    inside (r1.model) {
      case _: core.Model.Atom =>
        fail("not expected an Atom")
      case _ =>
        // OK
    }

    inside (r2.model) {
      case a: core.Model.Atom =>
        a.uuid should === (constUuid)
    }
  }

  "Non-ASCII digits" in {
    val strings = Vector(
      "꩒", // U+AA52 CHAM DIGIT TWO
      "꘢"  // U+A622 VAI DIGIT TWO
    )
    for (s <- strings) {
      checkFail[Byte](s)
      checkFail[Short](s)
      checkFail[Int](s)
      checkFail[Long](s)
      checkFail[Float](s)
      checkFail[Double](s)
      checkFail[BigInt](s)
      checkFail[BigDecimal](s"${s},0,146028888070")
    }
  }

  private[this] val pat = ".*non-ASCII character.*".r

  private[this] def checkFail[A: Atomic](s: String): Unit = {
    val res = Atomic[A].fromString(s)
    inside(res) {
      case Left(Atomic.InvalidData(pat())) => // OK
    }
  }

  "BitVector" in {
    val a = Atomic[BitVector]
    val bits = Vector(
      BitVector.zero,
      BitVector.one,
      bin"11",
      bin"11001",
      bin"000001110000",
      bin"1010101010111110101100011"
    )
    for (bv <- bits) {
      roundtripStr(bv)(a) should === (bv)
      roundtripBin(bv)(a) should === (bv)
    }
  }

  "Negative zero" - {

    "Float" in {
      val af = Atomic[Float]
      val nzf: Float = -0.0f
      // string:
      val s = af.stringRepr(nzf)
      s should === ("-0.0")
      val r = af.fromString(s).getOrElse(fail)
      r should === (nzf)
      assert(isNegZeroF(r))
      // binary:
      val b = af.binaryRepr(nzf)
      val r2 = af.fromBinary(b).getOrElse(fail)._1
      r2 should === (nzf)
      assert(isNegZeroF(r2))
    }

    "Double" in {
      val ad = Atomic[Double]
      val nzd: Double = -0.0d
      // string:
      val s = ad.stringRepr(nzd)
      s should === ("-0.0")
      val r = ad.fromString(s).getOrElse(fail)
      r should === (nzd)
      assert(isNegZeroD(r))
      // binary:
      val b = ad.binaryRepr(nzd)
      val r2 = ad.fromBinary(b).getOrElse(fail)._1
      r2 should === (nzd)
      assert(isNegZeroD(r2))
    }

    "Sanity check" in {
      assert(isNegZeroF(-0.0f))
      assert(!isNegZeroF(0.0f))
      assert(!isNegZeroF(-1.0f))
      assert(isNegZeroD(-0.0d))
      assert(!isNegZeroD(0.0d))
      assert(!isNegZeroD(-1.0d))
    }

    def isNegZeroF(f: Float): Boolean =
      (f == -0.0f) && (StrictMath.copySign(1.0f, f) == -1.0f)

    def isNegZeroD(d: Double): Boolean =
      (d == -0.0d) && (StrictMath.copySign(1.0d, d) == -1.0d)
  }

  // TODO: this is a can of worms ...
  // See java.lang.Double#doubleToLongBits,
  // doubleToRawLongBits, longBitsToDouble,
  // and the corresponding methods for Float.
  // "NaN and -NaN" - {
  // }

  "Infinity and -Infinity" - {

    "Float" in {
      val pInf1 = roundtripStr(Float.PositiveInfinity)
      pInf1 should === (Float.PositiveInfinity)
      assert(pInf1.isPosInfinity)
      val pInf2 = roundtripBin(Float.PositiveInfinity)
      pInf2 should === (Float.PositiveInfinity)
      assert(pInf2.isPosInfinity)
      val nInf1 = roundtripStr(Float.NegativeInfinity)
      nInf1 should === (Float.NegativeInfinity)
      assert(nInf1.isNegInfinity)
      val nInf2 = roundtripBin(Float.NegativeInfinity)
      nInf2 should === (Float.NegativeInfinity)
      assert(nInf2.isNegInfinity)
    }

    "Double" in {
      val pInf1 = roundtripStr(Double.PositiveInfinity)
      pInf1 should === (Double.PositiveInfinity)
      assert(pInf1.isPosInfinity)
      val pInf2 = roundtripBin(Double.PositiveInfinity)
      pInf2 should === (Double.PositiveInfinity)
      assert(pInf2.isPosInfinity)
      val nInf1 = roundtripStr(Double.NegativeInfinity)
      nInf1 should === (Double.NegativeInfinity)
      assert(nInf1.isNegInfinity)
      val nInf2 = roundtripBin(Double.NegativeInfinity)
      nInf2 should === (Double.NegativeInfinity)
      assert(nInf2.isNegInfinity)
    }
  }

  "BigDecimal with custom MathContext" in {
    val atc = Atomic[BigDecimal]
    val precision = 23
    val rounding = RoundingMode.UP
    val mc = new MathContext(precision, rounding)
    val bd = BigDecimal("123.456", mc)
    // string:
    val r1 = roundtripStr(bd)
    r1 should === (bd)
    r1.bigDecimal should === (bd.bigDecimal)
    r1.mc should === (bd.mc)
    // binary:
    val r2 = roundtripBin(bd)
    r2 should === (bd)
    r2.bigDecimal should === (bd.bigDecimal)
    r2.mc should === (bd.mc)
  }

  def roundtripStr[A](a: A)(implicit A: Atomic[A]): A =
    A.fromString(A.stringRepr(a)).getOrElse(fail)

  def roundtripBin[A](a: A)(implicit A: Atomic[A]): A = {
    val (a2, r) = A.fromBinary(A.binaryRepr(a)).getOrElse(fail)
    r.length should === (0L)
    a2
  }
}

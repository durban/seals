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

import java.{ lang => jl }
import java.util.UUID
import java.math.{ MathContext, RoundingMode }
import java.nio.ByteBuffer

import cats.implicits._

import scodec.bits._

import org.scalatest.compatible.Assertion

class BuiltinAtomSpec extends BaseSpec {

  import Model.Atom.atom

  "Built-in atoms" in {
    val atoms = Vector(
      // primitives:
      atom[Byte],
      atom[Short],
      atom[Char],
      atom[Int],
      atom[Long],
      atom[Float],
      atom[Double],
      atom[Boolean],
      atom[Unit],
      // std types:
      atom[String],
      atom[Symbol],
      atom[BigInt],
      atom[BigDecimal],
      atom[MathContext],
      atom[RoundingMode],
      atom[UUID],
      // scodec-bits:
      atom[ByteVector],
      atom[BitVector]
    )
    atoms.map(_.uuid).toSet should have size atoms.size.toLong
    atoms.foreach { a =>
      Atomic.registry.get(a.uuid).fold {
        fail("missing Atomic")
      } { b =>
        b shouldBe theSameInstanceAs (a)
      }
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

  "NaN should be canonicalized" - {

    "Float" in {
      val af = Atomic[Float]
      // make sure, that we're testing the
      // right thing: let's have 2 different NaNs:
      val cNan: Float = Float.NaN // the canonical one
      assert(cNan.isNaN)
      val oNan: Float = StrictMath.sqrt(-1.0).toFloat // a different one
      assert(oNan.isNaN)
      // make sure they're different:
      jl.Float.floatToRawIntBits(cNan) should !== (jl.Float.floatToRawIntBits(oNan))
      // string
      af.stringRepr(cNan) should === ("NaN")
      af.stringRepr(oNan) should === ("NaN")
      sameBitsF(roundtripStr(cNan), cNan)
      sameBitsF(roundtripStr(oNan), cNan) // canonicalized
      // binary
      sameBitsF(roundtripBin(cNan), cNan)
      sameBitsF(roundtripBin(oNan), cNan) // canonicalized
      // non-canonical NaN-s should be rejected:
      val buf = ByteBuffer.allocate(4).putFloat(oNan)
      buf.rewind()
      val oBin = ByteVector.view(buf)
      val expMsg = "-*non-canonical Float NaN.*".r
      inside(af.fromBinary(oBin)) {
        case Left(Atomic.InvalidData(expMsg())) => // OK
      }
    }

    "Double" in {
      val ad = Atomic[Double]
      // make sure, that we're testing the
      // right thing: let's have 2 different NaNs:
      val cNan: Double = Double.NaN // the canonical one
      assert(cNan.isNaN)
      val oNan: Double = StrictMath.sqrt(-1.0) // a different one
      assert(oNan.isNaN)
      // make sure they're different:
      jl.Double.doubleToRawLongBits(cNan) should !== (jl.Double.doubleToRawLongBits(oNan))
      // string
      ad.stringRepr(cNan) should === ("NaN")
      ad.stringRepr(oNan) should === ("NaN")
      sameBitsD(roundtripStr(cNan), cNan)
      sameBitsD(roundtripStr(oNan), cNan) // canonicalized
      // binary
      sameBitsD(roundtripBin(cNan), cNan)
      sameBitsD(roundtripBin(oNan), cNan) // canonicalized
      // non-canonical NaN-s should be rejected:
      val buf = ByteBuffer.allocate(8).putDouble(oNan)
      buf.rewind()
      val oBin = ByteVector.view(buf)
      val expMsg = "-*non-canonical Double NaN.*".r
      inside(ad.fromBinary(oBin)) {
        case Left(Atomic.InvalidData(expMsg())) => // OK
      }
    }

    def sameBitsF(a: Float, b: Float): Assertion = {
      assert(a.isNaN)
      assert(b.isNaN)
      jl.Float.floatToRawIntBits(a) should === (jl.Float.floatToRawIntBits(b))
    }

    def sameBitsD(a: Double, b: Double): Assertion = {
      assert(a.isNaN)
      assert(b.isNaN)
      jl.Double.doubleToRawLongBits(a) should === (jl.Double.doubleToRawLongBits(b))
    }
  }

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
}

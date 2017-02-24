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
      atom[BigInt],
      atom[BigDecimal],
      atom[MathContext],
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
      val r = af.fromString(s).fold(err => fail(err.msg), a => a)
      r should === (nzf)
      assert(isNegZeroF(r))
      // binary:
      val b = af.binaryRepr(nzf)
      val r2 = af.fromBinary(b).fold(err => fail(err.msg), a => a._1)
      r2 should === (nzf)
      assert(isNegZeroF(r2))
    }

    "Double" in {
      val ad = Atomic[Double]
      val nzd: Double = -0.0d
      // string:
      val s = ad.stringRepr(nzd)
      s should === ("-0.0")
      val r = ad.fromString(s).fold(err => fail(err.msg), a => a)
      r should === (nzd)
      assert(isNegZeroD(r))
      // binary:
      val b = ad.binaryRepr(nzd)
      val r2 = ad.fromBinary(b).fold(err => fail(err.msg), a => a._1)
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

  "Primitive representations" - {

    def str[A](a: A)(implicit A: Atomic[A]): String =
      A.stringRepr(a)

    def bin[A](a: A)(implicit A: Atomic[A]): ByteVector =
      A.binaryRepr(a)

    def check[A: Atomic](as: Vector[A], strs: Vector[String], bins: Vector[ByteVector]): Unit = {
      as.length should === (strs.length)
      as.length should === (bins.length)
      for ((a, s, b) <- (as, strs, bins).zipped) {
        str(a) should === (s)
        bin(a) should === (b)
      }
    }

    "Byte" in {
      check[Byte](
        Vector(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue),
        Vector("-128", "-1", "0", "1", "127"),
        Vector(hex"80", hex"ff", hex"00", hex"01", hex"7f")
      )
    }

    "Short" in {
      check[Short](
        Vector(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue),
        Vector("-32768", "-1", "0", "1", "32767"),
        Vector(hex"8000", hex"ffff", hex"0000", hex"0001", hex"7fff")
      )
    }

    "Char" in {
      check[Char](
        Vector(Char.MinValue, 1.toChar, Char.MaxValue, 'a', 'z', 'A', 'Z'),
        Vector(Character.toString(Char.MinValue), Character.toString(1.toChar), Character.toString(Char.MaxValue), "a", "z", "A", "Z"),
        Vector(hex"0000", hex"0001", hex"ffff", hex"0061", hex"007a", hex"0041", hex"005a")
      )
    }

    "Int" in {
      check[Int](
        Vector(Int.MinValue, -1, 0, 1, Int.MaxValue),
        Vector("-2147483648", "-1", "0", "1", "2147483647"),
        Vector(hex"8000 0000", hex"ffff ffff", hex"0000 0000", hex"0000 0001", hex"7fff ffff")
      )
    }

    "Long" in {
      check[Long](
        Vector(Long.MinValue, -1L, 0L, 1L, Long.MaxValue),
        Vector("-9223372036854775808", "-1", "0", "1", "9223372036854775807"),
        Vector(hex"8000 0000 0000 0000", hex"ffff ffff ffff ffff", hex"0000 0000 0000 0000", hex"0000 0000 0000 0001", hex"7fff ffff ffff ffff")
      )
    }

    "Float" in {
      check[Float](
        Vector(
          Float.MinValue, -123.456f, -1.0f, 0.0f, Float.MinPositiveValue,
          1.0f, 999.999f, Float.MaxValue, Float.NaN, Float.NegativeInfinity,
          Float.PositiveInfinity, java.lang.Float.MIN_NORMAL
        ),
        Vector(
          "-3.4028235E38", "-123.456", "-1.0", "0.0", "1.4E-45",
          "1.0", "999.999", "3.4028235E38", "NaN", "-Infinity",
          "Infinity", "1.17549435E-38"
        ),
        Vector(
          hex"ff7f ffff", hex"c2f6 e979", hex"bf80 0000", hex"0000 0000", hex"0000 0001",
          hex"3f80 0000", hex"4479 fff0", hex"7f7fffff", hex"7fc0 0000", hex"ff80 0000",
          hex"7f80 0000", hex"0080 0000"
        )
      )
    }

    "Double" in {
      check[Double](
        Vector(
          Double.MinValue, -123.456d, -1.0d, 0.0d, Double.MinPositiveValue,
          1.0d, 999.999d, Double.MaxValue, Double.NaN, Double.NegativeInfinity,
          Double.PositiveInfinity, java.lang.Double.MIN_NORMAL, 1.111111111111111d, scala.math.Pi, scala.math.E
        ),
        Vector(
          "-1.7976931348623157E308", "-123.456", "-1.0", "0.0", "4.9E-324",
          "1.0", "999.999", "1.7976931348623157E308", "NaN", "-Infinity",
          "Infinity", "2.2250738585072014E-308", "1.111111111111111", "3.141592653589793", "2.718281828459045"
        ),
        Vector(
          hex"ffef ffff ffff ffff", hex"c05e dd2f 1a9f be77", hex"bff0 0000 0000 0000", hex"0000 0000 0000 0000", hex"0000 0000 0000 0001",
          hex"3ff0 0000 0000 0000", hex"408f 3ffd f3b6 45a2", hex"7fef ffff ffff ffff", hex"7ff8 0000 0000 0000", hex"fff0 0000 0000 0000",
          hex"7ff0 0000 0000 0000", hex"0010 0000 0000 0000", hex"3ff1 c71c 71c7 1c71", hex"4009 21fb 5444 2d18", hex"4005 bf0a 8b14 5769"
        )
      )
    }

    "Boolean" in {
      check[Boolean](
        Vector(true, false),
        Vector("true", "false"),
        Vector(hex"01", hex"00")
      )
    }

    "Unit" in {
      check[Unit](
        Vector(()),
        Vector("()"),
        Vector(ByteVector.empty)
      )
    }
  }
}

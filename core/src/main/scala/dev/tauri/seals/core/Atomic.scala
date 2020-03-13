/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

package dev.tauri.seals
package core

import java.util.UUID
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8

import scala.util.Try

import cats.Eq
import cats.implicits._

import scodec.bits._

trait Atomic[A] extends Serializable { this: Singleton =>

  def stringRepr(a: A): String

  def fromString(s: String): Either[Atomic.Error, A]

  def binaryRepr(a: A): ByteVector

  def fromBinary(b: ByteVector): Either[Atomic.Error, (A, ByteVector)]

  def description: String

  def uuid: UUID

  final lazy val atom: Model.Atom =
    Model.Atom(uuid, description)

  final override def equals(that: Any): Boolean = that match {
    case that: AnyRef => this eq that
    case _ => false
  }

  final override def hashCode: Int =
    System.identityHashCode(this)
}

object Atomic {

  sealed trait Error {
    def msg: String
  }

  object Error {

    def apply(msg: String): Error =
      InvalidData(msg)

    implicit val eqForAtomicError: Eq[Error] =
      Eq.fromUniversalEquals
  }

  final case class InsufficientData(expBytes: Long, actBytes: Long) extends Error {
    override def msg: String = sh"insufficient data: expected ${expBytes} bytes, got ${actBytes}"
  }

  final case class InvalidData(msg: String) extends Error

  abstract class Derived[A, B]()(implicit base: Atomic[B]) extends Atomic[A] { this: Singleton =>

    protected def from(b: B): Either[Error, A]

    protected def to(a: A): B

    final override def stringRepr(a: A): String =
      base.stringRepr(to(a))

    final override def fromString(s: String): Either[Error, A] =
      base.fromString(s) flatMap from

    final override def binaryRepr(a: A): ByteVector =
      base.binaryRepr(to(a))

    final override def fromBinary(b: ByteVector): Either[Error, (A, ByteVector)] = {
      for {
        br <- base.fromBinary(b)
        (b, r) = br
        a <- from(b)
      } yield (a, r)
    }
  }

  trait FallbackString[A] { this: Atomic[A] with Singleton =>

    final override def stringRepr(a: A): String =
      binaryRepr(a).toBase64(Bases.Alphabets.Base64Url)

    final override def fromString(s: String): Either[Error, A] = {
      for {
        bv <- ByteVector.fromBase64Descriptive(s, Bases.Alphabets.Base64Url).leftMap(Error(_))
        abv <- fromBinary(bv)
        a <- abv match {
          case (a, ByteVector.empty) => Right(a)
          case (_, r) => Left(Error(sh"leftover bytes: ${r}"))
        }
      } yield a
    }
  }

  trait FallbackBinary[A] { this: Atomic[A] with Singleton =>

    final override def binaryRepr(a: A): ByteVector = {
      val arr = stringRepr(a).getBytes(UTF_8)
      encodeLength32(arr.length) ++ ByteVector.view(arr)
    }

    final override def fromBinary(b: ByteVector): Either[Error, (A, ByteVector)] = {
      for {
        lenAndRest <- decodeLength32(b)
        (n, rest) = lenAndRest
        bvs <- trySplit(rest, n.toLong)
        (d, r) = bvs
        s <- d.decodeUtf8.leftMap(ex => Error(sh"${ex.getClass.getSimpleName}: ${ex.getMessage}"))
        a <- fromString(s)
      } yield (a, r)
    }
  }

  abstract class ForEnum[A](implicit A: EnumLike[A]) extends Atomic[A] { this: Singleton =>

    final override def stringRepr(a: A): String =
      A.name(a)

    final override def fromString(s: String): Either[Error, A] =
      A.fromName(s).leftMap(Error(_))

    final override def binaryRepr(a: A): ByteVector =
      encodeLength32(A.index(a))

    final override def fromBinary(b: ByteVector): Either[Error, (A, ByteVector)] = {
      for {
        ir <- decodeLength32(b)
        (idx, rest) = ir
        value <- A.fromIndex(idx).leftMap(Error(_))
      } yield (value, rest)
    }
  }

  private final def trySplit(b: ByteVector, n: Long): Either[Error, (ByteVector, ByteVector)] = {
    if (n < 0) Left(InvalidData(sh"negative length: ${n}"))
    else if (n > b.length) Left(InsufficientData(n, b.length))
    else Right(b.splitAt(n))
  }

  private final def encodeLength32(n: Int): ByteVector = {
    require(n >= 0, "negative length or index")
    ByteVector.fromInt(n, 4)
  }

  private final def encodeLength64(n: Long): ByteVector = {
    require(n >= 0L, "negative length or index")
    ByteVector.fromLong(n, 8)
  }

  private final def decodeLength32(b: ByteVector): Either[Error, (Int, ByteVector)] = {
    for {
      bvs <- trySplit(b, 4)
      (l, r) = bvs
      n <- l.toInt(signed = true) match {
        case n if n < 0 => Left(Error(sh"negative encoded length or index: $n"))
        case n => Right(n)
      }
    } yield (n, r)
  }

  private final def decodeLength64(b: ByteVector): Either[Error, (Long, ByteVector)] = {
    for {
      bvs <- trySplit(b, 8)
      (l, r) = bvs
      n <- l.toLong(signed = true) match {
        case n if n < 0 => Left(Error(sh"negative encoded length or index: $n"))
        case n => Right(n)
      }
    } yield (n, r)
  }

  def apply[A](implicit instance: Atomic[A]): Atomic[A] =
    instance

  implicit def eqForAtomic[A]: Eq[Atomic[A]] =
    Eq.instance(_ eq _)

  // Built-in instances:

  private sealed abstract class SimpleAtomic[A](
    override val description: String,
    id: UUID,
    sr: A => String,
    fs: String => Either[Error, A]
  ) extends Atomic[A] { this: Singleton =>

    final override val uuid: UUID =
      id

    final override def fromString(s: String): Either[Error, A] =
      fs(s)

    final override def stringRepr(a: A): String =
      sr(a)
  }

  private sealed abstract class ConstLenAtomic[A](
    desc: String,
    idStr: UUID,
    sr: A => String,
    fs: String => Either[Error, A],
    len: Int,
    br: (ByteBuffer, A) => Unit,
    fb: ByteBuffer => A
  ) extends SimpleAtomic[A](desc, idStr, sr, fs) { this: Singleton =>

    final override def binaryRepr(a: A): ByteVector = {
      val buf = ByteBuffer.allocate(len)
      br(buf, a)
      (buf : java.nio.Buffer).rewind() // upcast for JVM 8 compatibility
      ByteVector.view(buf)
    }

    final override def fromBinary(b: ByteVector): Either[Error, (A, ByteVector)] = {
      for {
        bvs <- trySplit(b, len.toLong)
        (d, r) = bvs
      } yield (fb(d.toByteBuffer), r)
    }
  }

  private[this] val isAscii: java.util.function.IntPredicate = {
    new java.util.function.IntPredicate with Serializable {
      final override def test(value: Int): Boolean =
        value < 128
    }
  }

  private[this] def tryParseAscii[A](parse: String => A): String => Either[Error, A] = { s =>
    if (s.codePoints.allMatch(isAscii)) {
      Either.catchNonFatal(parse(s)).leftMap(ex => Error(sh"${ex.getClass.getName}: ${ex.getMessage}"))
    } else {
      Left(InvalidData(sh"string contains non-ASCII character: '${s}'"))
    }
  }

  private[this] def fromTry[A](t: Try[A]): Either[Error, A] =
    Either.fromTry(t).leftMap(ex => Error(sh"${ex.getClass.getName}: ${ex.getMessage}"))

  // Primitives:

  implicit val builtinByte: Atomic[Byte] =
    SimpleByte

  private object SimpleByte extends ConstLenAtomic[Byte](
    "Byte",
    uuid"0a2d603f-b8fd-4f73-a102-3bd958d4ee22",
    _.toString,
    tryParseAscii(_.toByte),
    1,
    _ put _,
    _.get
  )

  implicit val builtinShort: Atomic[Short] =
    SimpleShort

  private object SimpleShort extends ConstLenAtomic[Short](
    "Short",
    uuid"00d9c457-b71a-45d5-8ee8-1ecfc6af2111",
    _.toString,
    tryParseAscii(_.toShort),
    2,
    _ putShort _,
    _.getShort
  )

  implicit val builtinChar: Atomic[Char] =
    SimpleChar

  private object SimpleChar extends ConstLenAtomic[Char](
    "Char",
    uuid"9d28d655-b16d-475d-87ac-295852deb4bc",
    String.valueOf(_),
    s => {
      if (s.length === 1) {
        Right(s(0))
      } else {
        Left(Error(sh"not a single Char: '${s}'"))
      }
    },
    2,
    _ putChar _,
    _.getChar
  )

  implicit val builtinInt: Atomic[Int] =
    SimpleInt

  private object SimpleInt extends ConstLenAtomic[Int](
    "Int",
    uuid"d9bfd653-c875-4dd0-8287-b806ee6eb85b",
    _.toString,
    tryParseAscii(_.toInt),
    4,
    _ putInt _,
    _.getInt
  )

  implicit val builtinLong: Atomic[Long] =
    SimpleLong

  private object SimpleLong extends ConstLenAtomic[Long](
    "Long",
    uuid"44e10ec2-ef7a-47b8-9851-7a5fe18a056a",
    _.toString,
    tryParseAscii(_.toLong),
    8,
    _ putLong _,
    _.getLong
  )

  implicit val builtinFloat: Atomic[Float] =
    SimpleFloat

  private object SimpleFloat extends SimpleAtomic[Float](
    "Float",
    uuid"13663ca9-1652-4e4b-8c88-f7a137773b75",
    _.toString,
    tryParseAscii(_.toFloat)
  ) {

    private[this] final val len = 4

    private[this] final val canonicalNanBits =
      java.lang.Float.floatToRawIntBits(Float.NaN)

    override def binaryRepr(a: Float): ByteVector = {
      val buf = ByteBuffer.allocate(len)
      // canonicalize NaNs:
      buf.putFloat(if (a.isNaN) Float.NaN else a)
      buf.rewind()
      ByteVector.view(buf)
    }

    override def fromBinary(b: ByteVector): Either[Error, (Float, ByteVector)] = {
      for {
        bvs <- trySplit(b, len.toLong)
        (d, r) = bvs
        a <- {
          val a = d.toByteBuffer.getFloat
          if (a.isNaN) {
            // must be the canonical NaN:
            val bits = java.lang.Float.floatToRawIntBits(a)
            if (bits === canonicalNanBits) Right(a)
            else Left(InvalidData(sh"non-canonical Float NaN: ${bits}"))
          } else {
            Right(a)
          }
        }
      } yield (a, r)
    }
  }

  implicit val builtinDouble: Atomic[Double] =
    SimpleDouble

  private object SimpleDouble extends SimpleAtomic[Double](
    "Double",
    uuid"18c48a4d-48fd-4755-99c8-1b545e25edda",
    _.toString,
    tryParseAscii(_.toDouble)
  ) {

    private[this] final val len = 8

    private[this] final val canonicalNanBits =
      java.lang.Double.doubleToRawLongBits(Double.NaN)

    override def binaryRepr(a: Double): ByteVector = {
      val buf = ByteBuffer.allocate(len)
      // canonicalize NaNs:
      buf.putDouble(if (a.isNaN) Double.NaN else a)
      buf.rewind()
      ByteVector.view(buf)
    }

    override def fromBinary(b: ByteVector): Either[Error, (Double, ByteVector)] = {
      for {
        bvs <- trySplit(b, len.toLong)
        (d, r) = bvs
        a <- {
          val a = d.toByteBuffer.getDouble
          if (a.isNaN) {
            // must be the canonical NaN:
            val bits = java.lang.Double.doubleToRawLongBits(a)
            if (bits === canonicalNanBits) Right(a)
            else Left(InvalidData(sh"non-canonical Double NaN: ${bits}"))
          } else {
            Right(a)
          }
        }
      } yield (a, r)
    }
  }

  implicit val builtinBoolean: Atomic[Boolean] =
    SimpleBoolean

  private[this] final val FalseByte = hex"00"
  private[this] final val TrueByte = hex"01"

  private object SimpleBoolean extends SimpleAtomic[Boolean](
    "Boolean",
    uuid"88211913-75f1-4908-8cca-d78b5cca6f58",
    _.toString,
    s => {
      if (s === true.toString) Right(true)
      else if (s === false.toString) Right(false)
      else Left(Error(sh"Not a Boolean: '${s}'"))
    }
  ) {

    final override def binaryRepr(a: Boolean): ByteVector = {
      if (a) TrueByte
      else FalseByte
    }

    final override def fromBinary(b: ByteVector): Either[Error, (Boolean, ByteVector)] = {
      for {
        bvs <- trySplit(b, 1)
        (d, r) = bvs
        a <- d match {
          case FalseByte => Right(false)
          case TrueByte => Right(true)
          case _ => Left(Error(sh"invalid boolean: ${d}"))
        }
      } yield (a, r)
    }
  }

  implicit val builtinUnit: Atomic[Unit] =
    SimpleUnit

  private object SimpleUnit extends ConstLenAtomic[Unit](
    "Unit",
    uuid"d02152c8-c15b-4c74-9c3e-500af3dce57a",
    _ => "()",
    s => if (s === "()") Right(()) else Left(Error(sh"Not '()': '${s}'")),
    0,
    (_, _) => (),
    _ => ()
  )

  // Other standard types:

  implicit val builtinString: Atomic[String] =
    SimpleString

  private object SimpleString extends SimpleAtomic[String](
    "String",
    uuid"8cd4c733-4392-4a8c-9014-8decf160fffe",
    identity,
    Right(_)
  ) with FallbackBinary[String]

  implicit val builtinBigInt: Atomic[BigInt] =
    SimpleBigInt

  private object SimpleBigInt extends SimpleAtomic[BigInt](
    "BigInt",
    uuid"98ed3f70-7d50-4c18-9a07-caf57cfabced",
    _.toString,
    tryParseAscii(BigInt(_))
  ) {

    final override def binaryRepr(a: BigInt): ByteVector = {
      val arr = a.underlying.toByteArray
      encodeLength32(arr.length) ++ ByteVector.view(arr)
    }

    final override def fromBinary(b: ByteVector): Either[Error, (BigInt, ByteVector)] = {
      for {
        bvs <- decodeLength32(b)
        (n, rest) = bvs
        bvs <- if (n > 0) {
          trySplit(rest, n.toLong)
        } else {
          Left(Error("zero length prefix for BigInt"))
        }
        (d, r) = bvs
      } yield (BigInt(new java.math.BigInteger(d.toArray)), r)
    }
  }

  implicit val builtinUUID: Atomic[UUID] =
    SimpleUUID

  private object SimpleUUID extends ConstLenAtomic[UUID](
    "UUID",
    uuid"7eba2d39-7f93-4600-8901-1e4e5ec5e7ed",
    _.toString,
    s => fromTry(Try(UUID.fromString(s))), // TODO: reject extra leading zeros
    16,
    (buf, u) => {
      buf.putLong(u.getMostSignificantBits)
      buf.putLong(u.getLeastSignificantBits)
    },
    buf => {
      val most = buf.getLong
      val least = buf.getLong
      new UUID(most, least)
    }
  )

  // Types from scodec-bits:

  implicit val builtinByteVector: Atomic[ByteVector] =
    SimpleByteVector

  private object SimpleByteVector extends SimpleAtomic[ByteVector](
    "ByteVector",
    uuid"98d436cc-3421-4bff-a575-28c1952b976a",
    _.toBase64(Bases.Alphabets.Base64Url),
    s => ByteVector.fromBase64Descriptive(s, Bases.Alphabets.Base64Url).leftMap(Error(_))
  ) {

    override def binaryRepr(a: ByteVector): ByteVector =
      encodeLength64(a.length) ++ a

    override def fromBinary(b: ByteVector): Either[Error, (ByteVector, ByteVector)] = {
      for {
        lr <- decodeLength64(b)
        (l, r) = lr
        dr <- trySplit(r, l)
      } yield dr
    }
  }

  implicit val builtinBitVector: Atomic[BitVector] =
    SimpleBitVector

  private object SimpleBitVector extends SimpleAtomic[BitVector](
    "BitVector",
    uuid"a533de88-6ebc-45c4-a40c-29588a1a1ef1",
    _.toBin(Bases.Alphabets.Binary),
    s => BitVector.fromBinDescriptive(s, Bases.Alphabets.Binary).leftMap(Error(_))
  ) {

    override def binaryRepr(a: BitVector): ByteVector =
      encodeLength64(a.length) ++ a.bytes

    override def fromBinary(b: ByteVector): Either[Error, (BitVector, ByteVector)] = {
      for {
        lr <- decodeLength64(b)
        (l, r) = lr
        bytes = if ((l % 8L) > 0L) {
          (l / 8L) + 1
        } else {
          l / 8L
        }
        dr <- trySplit(r, bytes)
        (d, r) = dr
      } yield (d.bits.take(l), r)
    }
  }

  // Registry:

  private[seals] val registry: Map[UUID, Model.Atom] = Map(
    // primitives:
    entryOf[Byte],
    entryOf[Short],
    entryOf[Char],
    entryOf[Int],
    entryOf[Long],
    entryOf[Float],
    entryOf[Double],
    entryOf[Boolean],
    entryOf[Unit],
    // other standard types:
    entryOf[String],
    entryOf[BigInt],
    entryOf[UUID],
    // scodec-bits:
    entryOf[ByteVector],
    entryOf[BitVector]
  )

  private def entryOf[A](implicit a: Atomic[A]): (UUID, Model.Atom) =
    a.uuid -> a.atom
}

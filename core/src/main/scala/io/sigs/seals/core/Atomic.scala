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

  def fromString(s: String): Either[String, A]

  def binaryRepr(a: A): ByteVector

  def fromBinary(b: ByteVector): Either[String, (A, ByteVector)]

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

  trait FallbackBinary[A] { this: Atomic[A] with Singleton =>

    final override def binaryRepr(a: A): ByteVector = {
      val arr = stringRepr(a).getBytes(UTF_8)
      encodeLength(arr.length) ++ ByteVector.view(arr)
    }

    final override def fromBinary(b: ByteVector): Either[String, (A, ByteVector)] = {
      for {
        lenAndRest <- decodeLength(b)
        (n, rest) = lenAndRest
        bvs <- trySplit(rest, n.toLong)
        (d, r) = bvs
        s <- d.decodeUtf8.leftMap(ex => s"${ex.getClass.getSimpleName}: ${ex.getMessage}")
        a <- fromString(s)
      } yield (a, r)
    }
  }

  private final def trySplit(b: ByteVector, n: Long): Either[String, (ByteVector, ByteVector)] = {
    if (n < 0) Left(s"negative length: ${n}")
    else if (n > b.length) Left(s"insufficient length: cannot get ${n} bytes from a vector of ${b.length}")
    else Right(b.splitAt(n))
  }

  private final def encodeLength(i: Int): ByteVector = {
    require(i >= 0, "negative length")
    ByteVector.fromInt(i, 4)
  }

  private final def decodeLength(b: ByteVector): Either[String, (Int, ByteVector)] = {
    for {
      bvs <- trySplit(b, 4)
      (l, r) = bvs
      n <- l.toInt(signed = true) match {
        case n if n < 0 => Left(s"negative encoded length: $n")
        case n => Right(n)
      }
    } yield (n, r)
  }

  def apply[A](implicit instance: Atomic[A]): Atomic[A] =
    instance

  implicit def eqForAtomic[A]: Eq[Atomic[A]] =
    Eq.instance(_ eq _)

  // Built-in instances:

  sealed abstract class SimpleAtomic[A](
    override val description: String,
    idStr: String,
    sr: A => String,
    fs: String => Either[String, A]
  ) extends Atomic[A] { this: Singleton =>

    final override val uuid: UUID =
      UUID.fromString(idStr)

    final override def fromString(s: String): Either[String, A] =
      fs(s)

    final override def stringRepr(a: A): String =
      sr(a)
  }

  sealed abstract class ConstLenAtomic[A](
    desc: String,
    idStr: String,
    sr: A => String,
    fs: String => Either[String, A],
    len: Int,
    br: (ByteBuffer, A) => Unit,
    fb: ByteBuffer => A
  ) extends SimpleAtomic[A](desc, idStr, sr, fs) { this: Singleton =>

    final override def binaryRepr(a: A): ByteVector = {
      val buf = ByteBuffer.allocate(len)
      br(buf, a)
      buf.rewind()
      ByteVector.view(buf)
    }

    final override def fromBinary(b: ByteVector): Either[String, (A, ByteVector)] = {
      for {
        bvs <- trySplit(b, len.toLong)
        (d, r) = bvs
      } yield (fb(d.toByteBuffer), r)
    }
  }

  private[this] def fromTry[A](t: Try[A]): Either[String, A] =
    Either.fromTry(t).leftMap(ex => s"${ex.getClass.getName}: ${ex.getMessage}")

  // Primitives:

  implicit val builtinByte: Atomic[Byte] =
    SimpleByte

  private object SimpleByte extends ConstLenAtomic[Byte](
    "Byte",
    "0a2d603f-b8fd-4f73-a102-3bd958d4ee22",
    _.toString,
    s => fromTry(Try(s.toByte)),
    1,
    _ put _,
    _.get
  )

  implicit val builtinShort: Atomic[Short] =
    SimpleShort

  private object SimpleShort extends ConstLenAtomic[Short](
    "Short",
    "00d9c457-b71a-45d5-8ee8-1ecfc6af2111",
    _.toString,
    s => fromTry(Try(s.toShort)),
    2,
    _ putShort _,
    _.getShort
  )

  implicit val builtinChar: Atomic[Char] =
    SimpleChar

  private object SimpleChar extends ConstLenAtomic[Char](
    "Char",
    "9d28d655-b16d-475d-87ac-295852deb4bc",
    String.valueOf(_),
    s => {
      if (s.length === 1) {
        Right(s(0))
      } else {
        Left(s"not a single Char: '${s}'")
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
    "d9bfd653-c875-4dd0-8287-b806ee6eb85b",
    _.toString,
    s => fromTry(Try(s.toInt)),
    4,
    _ putInt _,
    _.getInt
  )

  implicit val builtinLong: Atomic[Long] =
    SimpleLong

  private object SimpleLong extends ConstLenAtomic[Long](
    "Long",
    "44e10ec2-ef7a-47b8-9851-7a5fe18a056a",
    _.toString,
    s => fromTry(Try(s.toLong)),
    8,
    _ putLong _,
    _.getLong
  )

  implicit val builtinFloat: Atomic[Float] =
    SimpleFloat

  private object SimpleFloat extends ConstLenAtomic[Float](
    "Float",
    "13663ca9-1652-4e4b-8c88-f7a137773b75",
    _.toString,
    s => fromTry(Try(s.toFloat)),
    4,
    _ putFloat _,
    _.getFloat
  )

  implicit val builtinDouble: Atomic[Double] =
    SimpleDouble

  private object SimpleDouble extends ConstLenAtomic[Double](
    "Double",
    "18c48a4d-48fd-4755-99c8-1b545e25edda",
    _.toString,
    s => fromTry(Try(s.toDouble)),
    8,
    _ putDouble _,
    _.getDouble
  )

  implicit val builtinBoolean: Atomic[Boolean] =
    SimpleBoolean

  private[this] final val FalseByte = hex"00"
  private[this] final val TrueByte = hex"01"

  private object SimpleBoolean extends SimpleAtomic[Boolean](
    "Boolean",
    "88211913-75f1-4908-8cca-d78b5cca6f58",
    _.toString,
    s => {
      if (s === true.toString) Right(true)
      else if (s === false.toString) Right(false)
      else Left(s"Not a Boolean: '${s}'")
    }
  ) {

    final override def binaryRepr(a: Boolean): ByteVector = {
      if (a) TrueByte
      else FalseByte
    }

    final override def fromBinary(b: ByteVector): Either[String, (Boolean, ByteVector)] = {
      for {
        bvs <- trySplit(b, 1)
        (d, r) = bvs
        a <- d match {
          case FalseByte => Right(false)
          case TrueByte => Right(true)
          case _ => Left(s"invalid boolean: ${d}")
        }
      } yield (a, r)
    }
  }

  implicit val builtinUnit: Atomic[Unit] =
    SimpleUnit

  private object SimpleUnit extends ConstLenAtomic[Unit](
    "Unit",
    "d02152c8-c15b-4c74-9c3e-500af3dce57a",
    _ => "()",
    s => if (s === "()") Right(()) else Left(s"Not '()': '${s}'"),
    0,
    (_, _) => (),
    _ => ()
  )

  // Other standard types:

  implicit val builtinString: Atomic[String] =
    SimpleString

  private object SimpleString extends SimpleAtomic[String](
    "String",
    "8cd4c733-4392-4a8c-9014-8decf160fffe",
    identity,
    Right(_)
  ) with FallbackBinary[String]

  implicit val builtinSymbol: Atomic[Symbol] =
    SimpleSymbol

  private object SimpleSymbol extends SimpleAtomic[Symbol](
    "Symbol",
    "8c750487-1a6b-4c99-b01a-f1392b8177ed",
    _.name,
    s => Right(Symbol(s))
  ) with FallbackBinary[Symbol]

  implicit val builtinBigInt: Atomic[BigInt] =
    SimpleBigInt

  private object SimpleBigInt extends SimpleAtomic[BigInt](
    "BigInt",
    "98ed3f70-7d50-4c18-9a07-caf57cfabced",
    _.toString,
    s => fromTry(Try(BigInt(s)))
  ) {

    final override def binaryRepr(a: BigInt): ByteVector = {
      val arr = a.underlying.toByteArray
      encodeLength(arr.length) ++ ByteVector.view(arr)
    }

    final override def fromBinary(b: ByteVector): Either[String, (BigInt, ByteVector)] = {
      for {
        bvs <- decodeLength(b)
        (n, rest) = bvs
        bvs <- if (n > 0) {
          trySplit(rest, n.toLong)
        } else {
          Left("zero length prefix for BigInt")
        }
        (d, r) = bvs
      } yield (BigInt(new java.math.BigInteger(d.toArray)), r)
    }
  }

  implicit val builtinBigDecimal: Atomic[BigDecimal] =
    SimpleBigDecimal

  private object SimpleBigDecimal extends SimpleAtomic[BigDecimal](
    "BigDecimal",
    "46317726-b42f-4147-9f99-fbbac2adce9a",
    _.toString,
    s => fromTry(Try(BigDecimal.exact(s)))
  ) with FallbackBinary[BigDecimal]

  implicit val builtinUUID: Atomic[UUID] =
    SimpleUUID

  private object SimpleUUID extends ConstLenAtomic[UUID](
    "UUID",
    "7eba2d39-7f93-4600-8901-1e4e5ec5e7ed",
    _.toString,
    s => fromTry(Try(UUID.fromString(s))),
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
    entryOf[Symbol],
    entryOf[BigInt],
    entryOf[BigDecimal],
    entryOf[UUID]
  )

  private def entryOf[A](implicit a: Atomic[A]): (UUID, Model.Atom) =
    a.uuid -> a.atom
}

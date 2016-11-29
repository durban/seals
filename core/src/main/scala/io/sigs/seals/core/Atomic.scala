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

import scala.util.Try

import cats.Eq
import cats.implicits._

trait Atomic[A] extends Serializable { this: Singleton =>

  def stringRepr(a: A): String

  def fromString(s: String): Either[String, A]

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

    override val uuid: UUID =
      UUID.fromString(idStr)

    override def fromString(s: String): Either[String, A] =
      fs(s)

    override def stringRepr(a: A): String =
      sr(a)
  }

  private[this] def fromTry[A](t: Try[A]): Either[String, A] =
    Either.fromTry(t).leftMap(ex => s"${ex.getClass.getName}: ${ex.getMessage}")

  // Primitives:

  implicit val builtinByte: Atomic[Byte] =
    SimpleByte

  private object SimpleByte extends SimpleAtomic[Byte](
    "Byte",
    "0a2d603f-b8fd-4f73-a102-3bd958d4ee22",
    _.toString,
    s => fromTry(Try(s.toByte))
  )

  implicit val builtinShort: Atomic[Short] =
    SimpleShort

  private object SimpleShort extends SimpleAtomic[Short](
    "Short",
    "00d9c457-b71a-45d5-8ee8-1ecfc6af2111",
    _.toString,
    s => fromTry(Try(s.toShort))
  )

  implicit val builtinChar: Atomic[Char] =
    SimpleChar

  private object SimpleChar extends SimpleAtomic[Char](
    "Char",
    "9d28d655-b16d-475d-87ac-295852deb4bc",
    String.valueOf(_),
    s => {
      if (s.length === 1) {
        Right(s(0))
      } else {
        Left(s"not a single Char: '${s}'")
      }
    }
  )

  implicit val builtinInt: Atomic[Int] =
    SimpleInt

  private object SimpleInt extends SimpleAtomic[Int](
    "Int",
    "d9bfd653-c875-4dd0-8287-b806ee6eb85b",
    _.toString,
    s => fromTry(Try(s.toInt))
  )

  implicit val builtinLong: Atomic[Long] =
    SimpleLong

  private object SimpleLong extends SimpleAtomic[Long](
    "Long",
    "44e10ec2-ef7a-47b8-9851-7a5fe18a056a",
    _.toString,
    s => fromTry(Try(s.toLong))
  )

  implicit val builtinFloat: Atomic[Float] =
    SimpleFloat

  private object SimpleFloat extends SimpleAtomic[Float](
    "Float",
    "13663ca9-1652-4e4b-8c88-f7a137773b75",
    _.toString,
    s => fromTry(Try(s.toFloat))
  )

  implicit val builtinDouble: Atomic[Double] =
    SimpleDouble

  private object SimpleDouble extends SimpleAtomic[Double](
    "Double",
    "18c48a4d-48fd-4755-99c8-1b545e25edda",
    _.toString,
    s => fromTry(Try(s.toDouble))
  )

  implicit val builtinBoolean: Atomic[Boolean] =
    SimpleBoolean

  private object SimpleBoolean extends SimpleAtomic[Boolean](
    "Boolean",
    "88211913-75f1-4908-8cca-d78b5cca6f58",
    _.toString,
    s => {
      if (s === true.toString) Right(true)
      else if (s === false.toString) Right(false)
      else Left(s"Not a Boolean: '${s}'")
    }
  )

  implicit val builtinUnit: Atomic[Unit] =
    SimpleUnit

  private object SimpleUnit extends SimpleAtomic[Unit](
    "Unit",
    "d02152c8-c15b-4c74-9c3e-500af3dce57a",
    _ => "()",
    s => if (s === "()") Right(()) else Left(s"Not '()': '${s}'")
  )

  // Other standard types:

  implicit val builtinString: Atomic[String] =
    SimpleString

  private object SimpleString extends SimpleAtomic[String](
    "String",
    "8cd4c733-4392-4a8c-9014-8decf160fffe",
    identity,
    Right(_)
  )

  implicit val builtinSymbol: Atomic[Symbol] =
    SimpleSymbol

  private object SimpleSymbol extends SimpleAtomic[Symbol](
    "Symbol",
    "8c750487-1a6b-4c99-b01a-f1392b8177ed",
    _.name,
    s => Right(Symbol(s))
  )

  implicit val builtinBigInt: Atomic[BigInt] =
    SimpleBigInt

  private object SimpleBigInt extends SimpleAtomic[BigInt](
    "BigInt",
    "98ed3f70-7d50-4c18-9a07-caf57cfabced",
    _.toString,
    s => fromTry(Try(BigInt(s)))
  )

  implicit val builtinBigDecimal: Atomic[BigDecimal] =
    SimpleBigDecimal

  private object SimpleBigDecimal extends SimpleAtomic[BigDecimal](
    "BigDecimal",
    "46317726-b42f-4147-9f99-fbbac2adce9a",
    _.toString,
    s => fromTry(Try(BigDecimal.exact(s)))
  )

  implicit val builtinUUID: Atomic[UUID] =
    SimpleUUID

  private object SimpleUUID extends SimpleAtomic[UUID](
    "UUID",
    "7eba2d39-7f93-4600-8901-1e4e5ec5e7ed",
    _.toString,
    s => fromTry(Try(UUID.fromString(s)))
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

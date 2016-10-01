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

import cats.implicits._

sealed trait BuiltinAtom[A] extends Serializable {
  private[core] def atom: Atom[A]
}

object BuiltinAtom {

  private final class Impl[A](override val atom: Atom[A])
    extends BuiltinAtom[A]

  private object Impl{
    def apply[A](atom: Atom[A]): BuiltinAtom[A] =
      new Impl(atom)
  }

  // Primitives:

  implicit val builtinByte: BuiltinAtom[Byte] =
    Impl(SimpleByte)

  private object SimpleByte extends SimpleAtom[Byte](
    "Byte",
    "0a2d603f-b8fd-4f73-a102-3bd958d4ee22",
    _.toString,
    s => Try(s.toByte).toOption
  )

  implicit val builtinShort: BuiltinAtom[Short] =
    Impl(SimpleShort)

  private object SimpleShort extends SimpleAtom[Short](
    "Short",
    "00d9c457-b71a-45d5-8ee8-1ecfc6af2111",
    _.toString,
    s => Try(s.toShort).toOption
  )

  implicit val builtinChar: BuiltinAtom[Char] =
    Impl(SimpleChar)

  private object SimpleChar extends SimpleAtom[Char](
    "Char",
    "9d28d655-b16d-475d-87ac-295852deb4bc",
    String.valueOf(_),
    s => {
      if (s.length === 1) {
        Some(s(0))
      } else {
        None
      }
    }
  )

  implicit val builtinInt: BuiltinAtom[Int] =
    Impl(SimpleInt)

  private object SimpleInt extends SimpleAtom[Int](
    "Int",
    "d9bfd653-c875-4dd0-8287-b806ee6eb85b",
    _.toString,
    s => Try(s.toInt).toOption
  )

  implicit val builtinLong: BuiltinAtom[Long] =
    Impl(SimpleLong)

  private object SimpleLong extends SimpleAtom[Long](
    "Long",
    "44e10ec2-ef7a-47b8-9851-7a5fe18a056a",
    _.toString,
    s => Try(s.toLong).toOption
  )

  implicit val builtinFloat: BuiltinAtom[Float] =
    Impl(SimpleFloat)

  private object SimpleFloat extends SimpleAtom[Float](
    "Float",
    "13663ca9-1652-4e4b-8c88-f7a137773b75",
    _.toString,
    s => Try(s.toFloat).toOption
  )

  implicit val builtinDouble: BuiltinAtom[Double] =
    Impl(SimpleDouble)

  private object SimpleDouble extends SimpleAtom[Double](
    "Double",
    "18c48a4d-48fd-4755-99c8-1b545e25edda",
    _.toString,
    s => Try(s.toDouble).toOption
  )

  implicit val builtinBoolean: BuiltinAtom[Boolean] =
    Impl(SimpleBoolean)

  private object SimpleBoolean extends SimpleAtom[Boolean](
    "Boolean",
    "88211913-75f1-4908-8cca-d78b5cca6f58",
    _.toString,
    s => {
      if (s === true.toString) Some(true)
      else if (s === false.toString) Some(false)
      else None
    }
  )

  implicit val builtinUnit: BuiltinAtom[Unit] =
    Impl(SimpleUnit)

  private object SimpleUnit extends SimpleAtom[Unit](
    "Unit",
    "d02152c8-c15b-4c74-9c3e-500af3dce57a",
    _ => "()",
    s => if (s === "()") Some(()) else None
  )

  // Other standard types:

  implicit val builtinString: BuiltinAtom[String] =
    Impl(SimpleString)

  private object SimpleString extends SimpleAtom[String](
    "String",
    "8cd4c733-4392-4a8c-9014-8decf160fffe",
    identity,
    Some(_)
  )

  implicit val builtinBigInt: BuiltinAtom[BigInt] =
    Impl(SimpleBigInt)

  private object SimpleBigInt extends SimpleAtom[BigInt](
    "BigInt",
    "98ed3f70-7d50-4c18-9a07-caf57cfabced",
    _.toString,
    s => Try(BigInt(s)).toOption
  )

  implicit val builtinBigDecimal: BuiltinAtom[BigDecimal] =
    Impl(SimpleBigDecimal)

  private object SimpleBigDecimal extends SimpleAtom[BigDecimal](
    "BigDecimal",
    "46317726-b42f-4147-9f99-fbbac2adce9a",
    _.toString,
    s => Try(BigDecimal.exact(s)).toOption
  )

  // Registry:

  private[seals] val registry: Map[UUID, Atom[_]] = Map(
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
    entryOf[BigDecimal]
  )

  private def entryOf[A](implicit a: Atom[A]): (UUID, Atom[A]) =
    a.uuid -> a
}

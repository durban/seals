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

sealed trait BuiltinAtom[A] extends Serializable {
  private[core] def atom: Atom[A]
}

object BuiltinAtom {

  private final class Impl[A](override val atom: Atom[A])
    extends BuiltinAtom[A]

  implicit val builtinInt: BuiltinAtom[Int] =
    new Impl(SimpleInt)

  implicit val builtinLong: BuiltinAtom[Long] =
    new Impl(SimpleLong)

  implicit val builtinString: BuiltinAtom[String] =
    new Impl(SimpleString)

  implicit val builtinFloat: BuiltinAtom[Float] =
    new Impl(SimpleFloat)

  private object SimpleInt extends SimpleAtom[Int](
    "Int",
    UUID.fromString("d9bfd653-c875-4dd0-8287-b806ee6eb85b")) {
    override def stringRepr(a: Int) = a.toString
    override def fromString(s: String) = Try(s.toInt).toOption
  }

  private object SimpleLong extends SimpleAtom[Long](
    "Long",
    UUID.fromString("44e10ec2-ef7a-47b8-9851-7a5fe18a056a")) {
    override def stringRepr(a: Long) = a.toString
    override def fromString(s: String) = Try(s.toLong).toOption
  }

  private object SimpleString extends SimpleAtom[String](
    "String",
    UUID.fromString("8cd4c733-4392-4a8c-9014-8decf160fffe")) {
    override def stringRepr(a: String) = a
    override def fromString(s: String) = Some(s)
  }

  private object SimpleFloat extends SimpleAtom[Float](
    "Float",
    UUID.fromString("13663ca9-1652-4e4b-8c88-f7a137773b75")) {
    override def stringRepr(a: Float) = a.toString
    override def fromString(s: String) = Try(s.toFloat).toOption
  }

  private[seals] val registry: Map[UUID, Atom[_]] = Map(
    entryOf[Int],
    entryOf[Long],
    entryOf[String],
    entryOf[Float]
  )

  private def entryOf[A](implicit a: Atom[A]): (UUID, Atom[A]) =
    a.uuid -> a
}

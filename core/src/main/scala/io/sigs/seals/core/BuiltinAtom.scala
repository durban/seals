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

  private object Impl{
    def apply[A](atom: Atom[A]): BuiltinAtom[A] =
      new Impl(atom)
  }

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

  implicit val builtinString: BuiltinAtom[String] =
    Impl(SimpleString)

  private object SimpleString extends SimpleAtom[String](
    "String",
    "8cd4c733-4392-4a8c-9014-8decf160fffe",
    identity,
    Some(_)
  )

  implicit val builtinFloat: BuiltinAtom[Float] =
    Impl(SimpleFloat)

  private object SimpleFloat extends SimpleAtom[Float](
    "Float",
    "13663ca9-1652-4e4b-8c88-f7a137773b75",
    _.toString,
    s => Try(s.toFloat).toOption
  )

  private[seals] val registry: Map[UUID, Atom[_]] = Map(
    entryOf[Int],
    entryOf[Long],
    entryOf[String],
    entryOf[Float]
  )

  private def entryOf[A](implicit a: Atom[A]): (UUID, Atom[A]) =
    a.uuid -> a
}

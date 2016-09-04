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
import cats.data.Xor
import cats.implicits._

trait AtomRegistry extends Serializable {

  def getAtom(id: UUID): Xor[String, Atom[_]]

  final def getAtom(s: String): Xor[String, Atom[_]] = {
    for {
      uuid <- Xor.fromTry(Try(UUID.fromString(s))).bimap(_.getMessage, identity)
      atom <- getAtom(uuid)
    } yield atom
  }

  final def + (a: Atom[_]): AtomRegistry = AtomRegistry.fromFunc { uuid =>
    if (uuid === a.uuid) Xor.right(a)
    else this.getAtom(uuid)
  }

  final def ++ (that: AtomRegistry): AtomRegistry = AtomRegistry.fromFunc { uuid =>
    this.getAtom(uuid).orElse(that.getAtom(uuid))
  }
}

object AtomRegistry {

  def fromMap(m: Map[UUID, Atom[_]]): AtomRegistry =
    fromFunction(m.get _)

  def fromFunc(f: UUID => Xor[String, Atom[_]]): AtomRegistry = new AtomRegistry {
    override def getAtom(id: UUID): Xor[String, Atom[_]] =
      f(id)
  }

  def fromFunction(f: UUID => Option[Atom[_]]): AtomRegistry = new AtomRegistry {
    override def getAtom(id: UUID): Xor[String, Atom[_]] =
      Xor.fromOption(f(id), s"not found Atom with id ${id}")
  }

  implicit val builtinAtomRegistry: AtomRegistry =
    fromMap(BuiltinAtom.registry)
}

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

import cats.implicits._

trait AtomRegistry extends Serializable {

  protected def map: Map[UUID, Atom[_]]

  final def getAtom(id: UUID): Either[String, Atom[_]] =
    Either.fromOption(map.get(id), s"not found Atom with id ${id}")

  final def getAtom(s: String): Either[String, Atom[_]] = {
    for {
      uuid <- Either.catchNonFatal(UUID.fromString(s)).bimap(_.getMessage, identity)
      atom <- getAtom(uuid)
    } yield atom
  }

  final def + (a: Atom[_]): AtomRegistry =
    new AtomRegistry.Impl(map.updated(a.uuid, a))

  final def ++ (that: AtomRegistry): AtomRegistry =
    new AtomRegistry.Impl(this.map ++ that.map)

  final override def equals(that: Any): Boolean = that match {
    case that: AtomRegistry =>
      this.map === that.map
    case _ =>
      false
  }

  final override def hashCode: Int =
    this.map.## ^ AtomRegistry.hashConst
}

object AtomRegistry {

  private final class Impl(protected override val map: Map[UUID, Atom[_]])
    extends AtomRegistry

  private final val hashConst = 0x48d400c7

  def fromMap(m: Map[UUID, Atom[_]]): AtomRegistry =
    new AtomRegistry.Impl(m)

  implicit val builtinAtomRegistry: AtomRegistry =
    fromMap(BuiltinAtom.registry)
}

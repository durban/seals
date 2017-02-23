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

package io.sigs

import java.util.UUID

import scala.language.implicitConversions

import scodec.bits.ByteVector

import io.sigs.seals.core.{ UUIDBuilder, NsUuid }

package object seals {

  type Reified[A] = core.Reified[A]
  val Reified = core.Reified

  type Model = core.Model
  val Model = core.Model

  type Kleene[F[_]] = core.Kleene[F]
  val Kleene = core.Kleene

  type Atomic[A] = core.Atomic[A]
  val Atomic = core.Atomic

  type Envelope[A] = core.Envelope[A]
  val Envelope = core.Envelope

  type Compat[A, B] = core.Compat[A, B]
  val Compat = core.Compat

  type Wire[A] = core.Wire[A]
  val Wire = core.Wire

  private[seals] implicit final class UUIDSyntax(private val self: UUID) extends AnyVal {
    def / (sub: UUID): UUIDBuilder = UUIDBuilder(self, Vector(NsUuid.bvFromUUID(sub)))
    def / (sub: ByteVector): UUIDBuilder = UUIDBuilder(self, Vector(sub))
  }

  private[seals] implicit def uuidLiteralSyntax(sc: StringContext): macros.UUIDSyntax =
    new macros.UUIDSyntax(sc)
}

/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
 * Copyright 2020 Nokia
 * SPDX-License-Identifier: Apache-2.0
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

package dev.tauri

import java.util.UUID

import scala.language.implicitConversions

import cats.Show

package object seals extends ScalaVersionCompat {

  type Reified[A] = core.Reified[A]
  val Reified = core.Reified

  // TODO: This seems to cause sporadic object initialization
  // TODO: problems (probably there is a cycle through `Model`).
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

  private[seals] implicit def uuidSyntax(self: UUID): core.UUIDUtils.UUIDSyntax =
    new core.UUIDUtils.UUIDSyntax(self)

  private[seals] implicit def uuidLiteralSyntax(sc: StringContext): macros.UUIDSyntax =
    new macros.UUIDSyntax(sc)

  private[seals] implicit class ShortShowSyntax(private val sc: StringContext) extends AnyVal {
    def sh(args: Show.Shown*): String = Show.ShowInterpolator(sc).show(args: _*)
  }
}

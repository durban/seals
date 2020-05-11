/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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

package dev.tauri.seals
package core

import java.util.UUID
import java.nio.charset.StandardCharsets

import scala.language.implicitConversions

import scodec.bits.ByteVector

final object UUIDUtils {

  implicit final class UUIDSyntax(private val self: UUID) extends AnyVal {
    def / (sub: UUID): UUIDBuilder = UUIDBuilder(self) / sub
    def / (sub: ByteVector): UUIDBuilder = UUIDBuilder(self) / sub
    def / (sub: String): UUIDBuilder = UUIDBuilder(self) / sub
  }

  final case class UUIDBuilder(namespace: UUID, name: Vector[ByteVector] = Vector.empty) {
    def / (sub: UUID): UUIDBuilder = copy(name = name :+ NsUUID.bvFromUUID(sub))
    def / (sub: ByteVector): UUIDBuilder = copy(name = name :+ sub)
    def / (sub: String): UUIDBuilder = copy(name = name :+ ByteVector.view(sub.getBytes(StandardCharsets.UTF_8)))
    def uuid: UUID = NsUUID.uuid5nestedBv(namespace, name: _*)
  }

  implicit def uuidLiteralSyntax(sc: StringContext): macros.UUIDSyntax =
    new macros.UUIDSyntax(sc)
}

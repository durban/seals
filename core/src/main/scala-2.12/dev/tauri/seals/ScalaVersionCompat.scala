/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

import scala.language.implicitConversions
import scala.runtime.Tuple2Zipped
import scala.collection.TraversableLike

import scala.collection.compat.{
  TrulyTraversableLikeExtensionMethods,
  Tuple2ZippedExtensionMethods
}

abstract class ScalaVersionCompat {

  // Scala 2.12 needs some collection syntax from the future:

  private[seals] implicit def lazyZipSyntax1[E, R](tl: TraversableLike[E, R]): TrulyTraversableLikeExtensionMethods[E, R] =
    new TrulyTraversableLikeExtensionMethods(tl)

  private[seals] implicit def lazyZipSyntax2[E1, R1, E2, R2](tz: Tuple2Zipped[E1, R1, E2, R2]): Tuple2ZippedExtensionMethods[E1, R1, E2, R2] =
    new Tuple2ZippedExtensionMethods(tz)
}

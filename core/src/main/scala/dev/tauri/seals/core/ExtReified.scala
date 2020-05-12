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

package dev.tauri.seals
package core

/**
 * Extension point for providing a `Reified` by
 * extending this trait (`Reified` cannot be extended
 * directly, since it is `sealed`).
 */
trait ExtReified[A] extends Serializable {
  type Mod <: Model
  type Fold[B, T]
  def reify: Reified.Aux[A, Mod, Fold]
}

final object ExtReified {

  type Aux[A, M <: Model, F[_, _]] = ExtReified[A] {
    type Mod = M
    type Fold[B, T] = F[B, T]
  }

  abstract class Simple[A, B](from: B => A, to: A => B)(implicit val B: Reified[B])
    extends ExtReified[A] {
    final override type Mod = B.Mod
    final override type Fold[X, T] = B.Fold[X, T]
    final override val reify: Reified.Aux[A, Mod, Fold] = B.imap[A](from)(to)
  }

  def apply[A](inst: ExtReified[A]): ExtReified.Aux[A, inst.Mod, inst.Fold] =
    inst

  def instance[A, B](to: A => B, from: B => A)(
    implicit B: Reified[B]
  ): ExtReified.Aux[A, B.Mod, B.Fold] = {
    new ExtReified[A] {
      final override type Mod = B.Mod
      final override type Fold[X, T] = B.Fold[X, T]
      final override val reify = B.imap[A](from)(to)
    }
  }
}

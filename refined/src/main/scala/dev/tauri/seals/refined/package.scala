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

import cats.Show

import shapeless.Nat
import shapeless.ops.nat.ToInt

package object refined extends AllInstances {

  implicit def catsShowForShapelessNat[N <: Nat](implicit ti: ToInt[N], si: Show[Int]): Show[N] =
    Show.show(_ => si.show(ti()))
}

trait AllInstances
  extends refined.Refinements
  with refined.ReifiedInstances

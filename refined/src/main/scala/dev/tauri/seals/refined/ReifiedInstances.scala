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
package refined

import eu.timepit.refined.api.RefType

import core.Refinement

object ReifiedInstances extends ReifiedInstances

trait ReifiedInstances {

  implicit def reifiedFromRefinedType[A, M <: Model, P, R[_, _]](
    implicit
    A: Reified.Aux2[A, M],
    @proof R: RefType[R],
    refinement: Refinement.Aux[R[A, P], A],
    canRefine: Model.CanBeRefined[M]
  ): Reified.Aux[R[A, P], M, A.Fold] = A.refined[R[A, P]](refinement)
}

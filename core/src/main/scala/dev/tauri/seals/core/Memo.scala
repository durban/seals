/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

import scala.collection.immutable

private[core] object Memo {

  type Memo[K] = immutable.Set[K]

  type IdMemo[K <: AnyRef] = IdentitySet[K]

  /**
   * Value-based memo (as defined by
   * calling `equals`). Doesn't call
   * `hashCode`.
   */
  def valMemo[K]: Memo[K] =
    immutable.ListSet.empty[K]

  /**
   * Object identity-based memo, calls
   * neither `hashCode` nor `equals`.
   */
  def idMemo[K <: AnyRef]: IdMemo[K] =
    IdentitySet.empty[K]
}

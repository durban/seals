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

trait Wire[A] {

  type Repr
  type Err

  def toWire(a: A): Either[Err, Repr]

  def fromWire(r: Repr): Either[Err, A]

  def reified: Reified[A]
}

object Wire {

  type Aux[A, R, E] = Wire[A] {
    type Repr = R
    type Err = E
  }
}

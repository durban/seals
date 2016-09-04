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

package io.sigs

package object seals {

  type Reified[A] = core.Reified[A]
  val Reified = core.Reified

  type Model = core.Model
  val Model = core.Model

  type Atom[A] = core.Atom[A]
  val Atom = core.Atom

  type Atomic[A] = core.Atomic[A]

  type Envelope[A] = core.Envelope[A]
  val Envelope = core.Envelope

  type Compat[A, B] = core.Compat[A, B]
  val Compat = core.Compat

  type AtomRegistry = core.AtomRegistry
  val AtomRegistry = core.AtomRegistry
}

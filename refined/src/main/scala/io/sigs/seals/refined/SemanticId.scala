/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

package io.sigs.seals
package refined

import cats.Show

import shapeless.Witness

import eu.timepit.refined.numeric._

import core.Refinement.Semantics

trait SemanticId[A] {

  def semantics: Semantics

  final def repr(r: String): String =
    semantics.desc(r)
}

object SemanticId {

  def apply[A](implicit sid: SemanticId[A]): SemanticId[A] =
    sid

  def mk[A](s: Semantics): SemanticId[A] = new SemanticId[A] {
    override val semantics = s
  }

  implicit def forGreater[N : Show : Reified](implicit wit: Witness.Aux[N]): SemanticId[Greater[N]] =
    mk[Greater[N]](Semantics.greater[N](wit.value))

  implicit def forLess[N : Show : Reified](implicit wit: Witness.Aux[N]): SemanticId[Less[N]] =
    mk[Less[N]](Semantics.less[N](wit.value))
}

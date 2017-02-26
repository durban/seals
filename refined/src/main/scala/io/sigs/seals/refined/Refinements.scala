/*
 * Copyright 2017 Daniel Urban
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

import cats.implicits._

import eu.timepit.refined.api.{ RefType, Validate }
import eu.timepit.refined.numeric.Positive

import core.Refinement

object Refinements extends Refinements {

  val euTimepitRefined = uuid"670341e5-c2f8-46e2-be9d-44fca48265f0"
}

trait Refinements {

  import Refinements._

  implicit def positive[R[_, _], A](implicit R: RefType[R], v: Validate[A, Positive]): Refinement.Aux[R[A, Positive], A] = {
    new Refinement[R[A, Positive]] {
      override type Repr = A
      override val uuid = (root / euTimepitRefined / "positive").uuid // TODO
      override def desc(r: String) = sh"${r} > 0"
      override def from(a: A): Either[String, R[A, Positive]] = R.refine[Positive](a)
      override def to(pa: R[A, Positive]): A = R.unwrap(pa)
    }
  }
}

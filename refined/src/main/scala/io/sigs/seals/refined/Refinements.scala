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

import eu.timepit.refined.api.{ RefType, Validate }

import core.Refinement

object Refinements extends Refinements

trait Refinements {

  implicit def forRefType[R[_, _], A, P](implicit R: RefType[R], v: Validate[A, P], id: SemanticId[P]): Refinement.Aux[R[A, P], A] = {
    new Refinement[R[A, P]] {
      override type Repr = A
      override val uuid = id.uuid
      override def desc(r: String) = id.repr(r)
      override def from(a: A): Either[String, R[A, P]] = R.refine[P](a)
      override def to(pa: R[A, P]): A = R.unwrap(pa)
    }
  }
}

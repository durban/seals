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

import eu.timepit.refined.api._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

import laws.CanonicalRepr
import tests.BaseSpec

class RefinedSpec extends BaseSpec {

  "Reified instances for refinement types" - {

    "Positive" in {
      val k: Int Refined Positive = 89
      val f = CanonicalRepr.fold[Int Refined Positive](k)
      f should === (CanonicalRepr.Atom("89"))
      CanonicalRepr.unfold[Int Refined Positive](f) should === (Right(k))
      CanonicalRepr.unfold[Int Refined Positive](CanonicalRepr.Atom("0")) match {
        case Left(_) => // OK
        case Right(l) => fail(s"unexpected result: ${l}")
      }
    }
  }
}

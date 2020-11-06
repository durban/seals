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
package laws

import scala.util.control.NonFatal
import cats.kernel.Eq
import cats.kernel.laws._
import cats.kernel.laws.discipline._
import org.scalacheck.{ Arbitrary, Prop }
import org.scalacheck.Prop._

object Rules extends Serialization {

  def serializable[A: Arbitrary]: (String, Prop) = {
    "serializable" -> forAll { (a: A) =>
      withCatchNonFatal {
        roundtripSer(a) : A
        Prop(Result(status = True))
      }
    }
  }

  def equalitySerializable[A : Arbitrary : Eq]: (String, Prop) = {
    "serialize-roundtrip-Eq" -> forAll { (a: A) =>
      withCatchNonFatal {
        val r: A = roundtripSer(a)
        r <-> a
      }
    }
  }

  def identitySerializable[A <: AnyRef : Arbitrary](a: A): (String, Prop) = {
    "serializable-roundtrip-identity" -> forAll { (a: A) =>
      withCatchNonFatal {
        val r: A = roundtripSer(a)
        Prop(Result(status = if (r eq a) True else False))
      }
    }
  }

  private def withCatchNonFatal(block: => Prop): Prop = {
    try {
      block
    } catch {
      case NonFatal(ex) =>
        Prop(Result(status = Exception(ex)))
      case ex: Throwable =>
        throw ex // workaround for -Xstrict-patmat-analysis problem
    }
  }
}

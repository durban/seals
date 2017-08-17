/*
 * Copyright 2016-2017 Daniel Urban and contributors listed in AUTHORS
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
package laws

import cats.{ ~>, Eq, Show }
import cats.implicits._

import org.typelevel.discipline.scalatest.Discipline

trait AbstractWireSpec[R, E] { this: Discipline =>

  import TestArbInstances.forTestData._
  import TestTypes.adts.defs.{ Adt1, Adt2 }
  import TestTypes.adts.recursive.IntList
  import TestTypes.adts.recursive.v2.{ IntList => IntListV2 }
  import TestTypes.collections.{ Adt, Cyclic }

  def mkWire[A](r: Reified[A]): Wire.Aux[A, R, E]

  def descE: String

  def descR: String

  implicit def equR: Eq[R]
  implicit def shwE: Show[E]

  val mkWireNt = λ[Reified ~> Wire.Aux[?, R, E]](
    r => mkWire(r)
  )

  checkAll(
    s"WireLaws[Int, $descR, $descE]",
    WireLaws[Int, R, E](mkWireNt).roundtrip
  )
  checkAll(
    s"WireLaws[String, $descR, $descE]",
    WireLaws[String, R, E](mkWireNt).roundtrip
  )
  checkAll(
    s"WireLaws[Vector[String], $descR, $descE]",
    WireLaws[Vector[String], R, E](mkWireNt).roundtrip
  )
  checkAll(
    s"WireLaws[List[IntList], $descR, $descE]",
    WireLaws[List[IntList], R, E](mkWireNt).roundtrip
  )
  checkAll(
    s"WireLaws[collections.Adt, $descR, $descE]",
    WireLaws[Adt, R, E](mkWireNt).roundtrip
  )
  checkAll(
    s"WireLaws[collections.Cyclic, $descR, $descE]",
    WireLaws[Cyclic, R, E](mkWireNt).roundtrip
  )

  checkAll(
    s"WireLaws[Adt1, $descR, $descE]+Adt2",
    WireLaws[Adt1, R, E](mkWireNt).roundtripCompat[Adt2]
  )
  checkAll(
    s"WireLaws[IntList, $descR, $descE]+v2",
    WireLaws[IntList, R, E](mkWireNt).roundtripCompat[IntListV2]
  )
}

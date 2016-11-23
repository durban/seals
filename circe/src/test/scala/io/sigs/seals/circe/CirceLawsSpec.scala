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

package io.sigs.seals
package circe

import cats.kernel.Eq
import cats.instances.all._

import org.scalacheck.Arbitrary

import io.sigs.seals.laws.ArbInstances
import io.sigs.seals.laws.TestArbInstances
import io.sigs.seals.laws.TestTypes

class CirceLawsSpec extends tests.BaseLawsSpec {

  import Codec._
  import ArbInstances.arbEnvelope
  import TestArbInstances.forTestData._

  checkParametricLaws[Int]("Int")
  checkParametricLaws[String]("String")
  checkParametricLaws[Vector[String]]("Vector[String]")
  checkParametricLaws[TestTypes.adts.defs.Adt1]("Adt1")
  checkParametricLaws[TestTypes.adts.recursive.IntList]("IntList")
  checkParametricLaws[List[TestTypes.adts.recursive.IntList]]("List[IntList]")
  checkParametricLaws[TestTypes.collections.Adt]("collections.Adt")

  def checkParametricLaws[A](name: String)(implicit a: Arbitrary[A], eq: Eq[A], r: Reified[A]): Unit = {
    checkAll(s"Envelope[$name].CirceLaws", claws.CirceLaws[Envelope[A]].roundtrip)
  }
}

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
package tests

import java.util.UUID

import cats.kernel.laws._
import cats.Eq

import org.scalacheck.Arbitrary

import io.sigs.seals.laws._

import TestArbInstances._
import TestArbInstances.forTestData._
import TestInstances.atomic._

class LawsSpec extends BaseLawsSpec {

  // TODO: check with other types too, not just these
  checkParametricLaws[TestTypes.adts.defs.Adt1]("Adt1")
  checkParametricLaws[TestTypes.adts.recursive.IntList]("IntList")

  checkAll("Model.AnyLaws.any", AnyLaws[Model].any)
  checkAll("Model.AnyLaws.equalitySerializability", AnyLaws[Model].equalitySerializability)
  checkAll("Model.OrderLaws.eqv", OrderLaws[Model].eqv)

  checkAll("Atomic[UUID].AtomicLaws.roundtrip", AtomicLaws[UUID].roundtrip)

  def checkParametricLaws[A](name: String)(implicit a: Arbitrary[A], e: Eq[A], r: Reified[A]): Unit = {
    checkAll(s"Envelope[$name].AnyLaws.any", AnyLaws[Envelope[A]].any)
    checkAll(s"Envelope[$name].AnyLaws.equalitySerializability", AnyLaws[Envelope[A]].equalitySerializability)
    checkAll(s"Envelope[$name].OrderLaws.eqv", OrderLaws[Envelope[A]].eqv)

    checkAll(s"Reified[$name].AnyLaws.any", AnyLaws[Reified[A]].any)
    checkAll(s"Reified[$name].AnyLaws.referenceEquality", AnyLaws[Reified[A]].referenceEquality)
    checkAll(s"Reified[$name].OrderLaws.eqv", OrderLaws[Reified[A]].eqv)
    checkAll(s"Reified[$name].ReifiedLaws.reified", ReifiedLaws[A].reified)
  }
}

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
package tests

import java.util.UUID
import java.math.{ MathContext, RoundingMode }
import java.time.{ DayOfWeek, Month }

import cats.kernel.laws.discipline._
import cats.laws.discipline.InvariantMonoidalTests
import cats.Eq
import cats.instances.all._

import shapeless.Nat

import scodec.bits._
import scodec.interop.cats._

import org.scalacheck.{ Arbitrary, Cogen }

import core.{ EnumLike, CanonicalRepr }
import laws._

class LawsSpec extends BaseLawsSpec {

  import forTestData._

  checkEnvelopeLaws[TestTypes.adts.defs.Adt1]("Adt1")
  checkEnvelopeLaws[TestTypes.adts.recursive.IntList]("IntList")

  checkReifiedLaws[Model, Int, String]("Model")
  checkReifiedLaws[TestTypes.adts.defs.Adt1, Int, Int]("Adt1")
  checkReifiedLaws[TestTypes.adts.recursive.IntList, Int, Int]("IntList")
  checkReifiedLaws[Envelope[TestTypes.adts.defs.Adt1], Int, Int]("Envelope[Adt1]")
  checkReifiedLaws[Envelope[TestTypes.adts.recursive.IntList], Int, Int]("Envelope[IntList]")
  checkReifiedLaws[List[String], Boolean, Float]("List[String]")
  checkReifiedLaws[Set[Int], String, Double]("Set[Int]")
  checkReifiedLaws[Map[Int, Float], String, Double]("Map[Int, Float]")
  checkReifiedLaws[Option[Int], String, Float]("Option[Int]")
  checkReifiedLaws[Month, String, Float]("java.time.Month")
  checkReifiedLaws[Symbol, Float, Option[Int]]("scala.Symbol")
  checkReifiedLaws[RoundingMode, Symbol, Int]("java.math.RoundingMode")
  checkReifiedLaws[BigDecimal, String, Float]("scala.BigDecimal")
  checkReifiedLaws[MathContext, String, Float]("java.math.MathContext")
  checkReifiedLaws[Nat._2, Symbol, Float]("shapeless.Nat._2")

  locally {
    import TestInstances.reified._
    checkReifiedLaws[UUID, Int, Int]("UUID")
  }

  checkAtomicLaws[Byte]("Byte")
  checkAtomicLaws[Short]("Short")
  checkAtomicLaws[Char]("Char")
  checkAtomicLaws[Int]("Int")
  checkAtomicLaws[Long]("Long")
  checkAtomicLaws[Float]("Float")
  checkAtomicLaws[Double]("Double")
  checkAtomicLaws[Boolean]("Boolean")
  checkAtomicLaws[Unit]("Unit")
  checkAtomicLaws[String]("String")
  checkAtomicLaws[BigInt]("BigInt")
  checkAtomicLaws[UUID]("UUID")
  checkAtomicLaws[ByteVector]("ByteVector")
  checkAtomicLaws[BitVector]("BitVector")

  checkAtomicLaws[Byte]("DerivedAtomicTester")(implicitly, implicitly, AtomicLaws.DerivedAtomicTester)
  checkAtomicLaws[Int]("FallbackStringTester")(implicitly, implicitly, AtomicLaws.FallbackStringTester)
  checkAtomicLaws[Int]("FallbackBinaryTester")(implicitly, implicitly, AtomicLaws.FallbackBinaryTester)
  checkAtomicLaws[DayOfWeek]("ForEnumTester")(implicitly, implicitly, AtomicLaws.ForEnumTester)

  locally {
    import TestInstances.atomic._
    checkAtomicLaws[MyUUID]("MyUUID")
    checkAtomicLaws[TestTypes.Whatever.type]("TestTypes.Whatever")
  }

  locally {
    import LimitedContainers._
    checkKleeneLaws[Vector, Int]("Vector, Int")
    checkKleeneLaws[Vector, TestTypes.adts.recursive.IntList]("Vector, IntList")
    checkKleeneLaws[List, Int]("List, Int")
    checkKleeneLaws[List, TestTypes.adts.recursive.IntList]("List, IntList")

    locally {
      import TestInstances.kleene._
      checkKleeneLaws[Stream, Int]("Stream, Int")
      checkKleeneLaws[Stream, TestTypes.adts.recursive.IntList]("Stream, IntList")
    }
  }

  checkEnumLikeLaws[java.math.RoundingMode]("RoundingMode")
  checkEnumLikeLaws[MyTestEnum]("MyTestEnum")
  checkEnumLikeLaws[MyTestEnumWithArgs]("MyTestEnumWithArgs")
  checkEnumLikeLaws[MyTestEnumWithOverloads]("MyTestEnumWithOverloads")
  checkEnumLikeLaws[MyTestEnumWithToString]("MyTestEnumWithToString")

  checkAll("Model.AnyLaws.any", AnyLaws[Model].any)
  checkAll("Model.AnyLaws.equalitySerializability", AnyLaws[Model].equalitySerializability)
  checkAll("Model.EqTests.eqv", EqTests[Model].eqv)

  checkCanonicalReprLaws()

  def checkEnvelopeLaws[A](name: String)(implicit a: Arbitrary[A], c: Cogen[A], e: Eq[A], r: Reified[A]): Unit = {
    checkAll(s"Envelope[$name].AnyLaws.any", AnyLaws[Envelope[A]].any)
    checkAll(s"Envelope[$name].AnyLaws.equalitySerializability", AnyLaws[Envelope[A]].equalitySerializability)
    checkAll(s"Envelope[$name].EqTests.eqv", EqTests[Envelope[A]].eqv)
  }

  def checkReifiedLaws[A, B, C](name: String)(
    implicit
    aa: Arbitrary[A],
    ab: Arbitrary[B],
    ac: Arbitrary[C],
    ca: Cogen[A],
    cb: Cogen[B],
    cc: Cogen[C],
    e: Eq[A],
    ra: Reified[A],
    rb: Reified[B],
    rc: Reified[C]
  ): Unit = {

    checkAll(s"Reified[$name].AnyLaws.serializability", AnyLaws[Reified[A]].serializability)
    checkAll(s"Reified[$name].AnyLaws.referenceEquality", AnyLaws[Reified[A]].referenceEquality)
    checkAll(s"Reified[$name].ReifiedLaws.reified", ReifiedLaws[A].reified)

    // these require a test Eq[Reified[X]] instance:

    checkAll(
      s"Reified[$name].EqTests.eqv",
      EqTests[Reified[A]](testEqForReified).eqv
    )

    // TODO: we pass these explicitly due to an ambiguous implicit
    checkAll(
      s"Reified[$name].InvariantMonoidal.invariantMonoidal",
      InvariantMonoidalTests[Reified].invariantMonoidal[A, B, C](
        aa,
        ab,
        ac,
        arbReified,
        arbReified,
        arbReified,
        ca,
        cb,
        cc,
        testEqForReified,
        testEqForReified,
        implicitly,
        testEqForReified,
        testEqForReified,
        testEqForReified
      )
    )
  }

  def checkKleeneLaws[F[_], A](name: String)(
    implicit
    arbA: Arbitrary[A],
    arbFA: Arbitrary[F[A]],
    kle: Kleene[F],
    equA: Eq[A],
    equFA: Eq[F[A]]
  ): Unit = {
    checkAll(s"Kleene[$name].AnyLaws.serializability", AnyLaws[Kleene[F]].serializability)
    checkAll(s"Kleene[$name].KleeneLaws.roundtrip", KleeneLaws[F, A].roundtrip)
  }

  def checkEnumLikeLaws[A](name: String)(
    implicit
    arb: Arbitrary[A],
    enu: EnumLike[A],
    equ: Eq[A]
  ): Unit = {
    checkAll(s"EnumLike[$name].AnyLaws.serializability", AnyLaws[EnumLike[A]].serializability)
    checkAll(s"EnumLike[$name].all", EnumLikeLaws[A].all)
  }

  def checkCanonicalReprLaws(): Unit = {
    checkAll("CanonicalRepr.Order", OrderTests[CanonicalRepr].order)
  }
}

/*
 * Copyright 2016-2017 Daniel Urban
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
import java.math.{ MathContext, RoundingMode }

import cats.kernel.laws._
import cats.laws.discipline.InvariantMonoidalTests
import cats.Eq
import cats.instances.all._

import scodec.bits._
import scodec.interop.cats._

import org.scalacheck.{ Arbitrary, Cogen }

import io.sigs.seals.laws._

class LawsSpec extends BaseLawsSpec {

  import forTestData._

  checkEnvelopeLaws[TestTypes.adts.defs.Adt1]("Adt1")
  checkEnvelopeLaws[TestTypes.adts.recursive.IntList]("IntList")

  checkReifiedLaws[Model, Int, String]("Model")
  checkReifiedLaws[TestTypes.adts.defs.Adt1, Int, Int]("Adt1")
  checkReifiedLaws[TestTypes.adts.recursive.IntList, Int, Int]("IntList")
  checkReifiedLaws[Envelope[TestTypes.adts.defs.Adt1], Int, Int]("Envelope[Adt1]")
  checkReifiedLaws[Envelope[TestTypes.adts.recursive.IntList], Int, Int]("Envelope[IntList]")

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
  checkAtomicLaws[Symbol]("Symbol")
  checkAtomicLaws[BigInt]("BigInt")
  checkAtomicLaws[BigDecimal]("BigDecimal")
  checkAtomicLaws[MathContext]("MathContext")
  checkAtomicLaws[RoundingMode]("RoundingMode")
  checkAtomicLaws[UUID]("UUID")
  checkAtomicLaws[ByteVector]("ByteVector")
  checkAtomicLaws[BitVector]("BitVector")

  checkAtomicLaws[Byte]("DerivedAtomicTester")(implicitly, implicitly, AtomicLaws.DerivedAtomicTester)
  checkAtomicLaws[Int]("FallbackStringTester")(implicitly, implicitly, AtomicLaws.FallbackStringTester)
  checkAtomicLaws[Int]("FallbackBinaryTester")(implicitly, implicitly, AtomicLaws.FallbackBinaryTester)

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

  checkAll("Model.AnyLaws.any", AnyLaws[Model].any)
  checkAll("Model.AnyLaws.equalitySerializability", AnyLaws[Model].equalitySerializability)
  checkAll("Model.OrderLaws.eqv", OrderLaws[Model].eqv)

  def checkEnvelopeLaws[A](name: String)(implicit a: Arbitrary[A], c: Cogen[A], e: Eq[A], r: Reified[A]): Unit = {
    checkAll(s"Envelope[$name].AnyLaws.any", AnyLaws[Envelope[A]].any)
    checkAll(s"Envelope[$name].AnyLaws.equalitySerializability", AnyLaws[Envelope[A]].equalitySerializability)
    checkAll(s"Envelope[$name].OrderLaws.eqv", OrderLaws[Envelope[A]].eqv)
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
      s"Reified[$name].OrderLaws.eqv",
      OrderLaws[Reified[A]](testEqForReified, arbReified, cogenReified).eqv
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
}

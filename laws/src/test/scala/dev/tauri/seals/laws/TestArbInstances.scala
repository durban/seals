/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

import java.util.UUID

import shapeless.{ Nat, Witness }
import shapeless.ops.nat.ToInt

import org.scalacheck.{ Arbitrary, Gen, Cogen }
import org.scalacheck.rng.Seed
import org.scalacheck.derive.Recursive

object TestArbInstances extends TestArbInstances

trait TestArbInstances extends ArbInstances {

  object forTestData {

    implicit val arbDefsAdt1: Arbitrary[TestTypes.adts.defs.Adt1] = {
      import org.scalacheck.ScalacheckShapeless._
      derivedArbitrary(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[TestTypes.adts.defs.Adt1]
      )
    }

    implicit val cogenDefsAdt1: Cogen[TestTypes.adts.defs.Adt1] = {
      import org.scalacheck.ScalacheckShapeless._
      derivedCogen(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkCogen[TestTypes.adts.defs.Adt1]
      )
    }

    implicit val arbDefsAdt2: Arbitrary[TestTypes.adts.defs.Adt2] = {
      import org.scalacheck.ScalacheckShapeless._
      derivedArbitrary(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[TestTypes.adts.defs.Adt2]
      )
    }

    implicit val recDefsIntList: Recursive[TestTypes.adts.recursive.IntList] =
      Recursive(Gen.const(TestTypes.adts.recursive.IntNil))

    implicit val arbDefsIntList: Arbitrary[TestTypes.adts.recursive.IntList] = {
      import org.scalacheck.ScalacheckShapeless._
      derivedArbitrary(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[TestTypes.adts.recursive.IntList]
      )
    }

    implicit lazy val cogenDefsIntList: Cogen[TestTypes.adts.recursive.IntList] = {
      def perturb(seed: Seed, l: TestTypes.adts.recursive.IntList): Seed = {
        l match {
          case TestTypes.adts.recursive.IntNil =>
            seed.reseed(0x80a894f4f89b314aL)
          case TestTypes.adts.recursive.IntCons(h, t) =>
            perturb(Cogen[Int].perturb(seed, h), t)
        }
      }
      Cogen(perturb _)
    }

    implicit val recDefsIntListV2: Recursive[TestTypes.adts.recursive.v2.IntList] =
      Recursive(Gen.const(TestTypes.adts.recursive.v2.IntNil))

    implicit val arbDefsIntListV2: Arbitrary[TestTypes.adts.recursive.v2.IntList] = {
      import org.scalacheck.ScalacheckShapeless._
      derivedArbitrary(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[TestTypes.adts.recursive.v2.IntList]
      )
    }

    implicit val recCollCyclic: Recursive[TestTypes.collections.Cyclic] =
      Recursive(Gen.const(TestTypes.collections.CyB))

    implicit val arbCollCyclic: Arbitrary[TestTypes.collections.Cyclic] = {
      import org.scalacheck.ScalacheckShapeless._
      derivedArbitrary(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[TestTypes.collections.Cyclic]
      )
    }
  }

  /** Inserts custom Atoms besides the built-in ones */
  override def unrefinedArbModelAtom: Arbitrary[Model.Atom] = Arbitrary {
    import TestInstances.atomic._
    import TestTypes.Whatever
    Gen.oneOf(
      super.unrefinedArbModelAtom.arbitrary,
      Gen.oneOf[Model.Atom](Model.Atom.atom[UUID], Model.Atom.atom[Whatever.type])
    )
  }

  /** Inserts cyclic and otherwise interesting Models besides the generated ones */
  implicit override def arbModel: Arbitrary[Model] = Arbitrary {
    Gen.oneOf(
      super.arbModel.arbitrary,
      Gen.oneOf[Model](
        TestTypes.adts.iso.Adt1.expModel,
        TestTypes.adts.recursive.IntList.expModel,
        TestTypes.custom.WithUuid.expModel
      )
    )
  }

  implicit def arbShapelessNat[N <: Nat](implicit wit: Witness.Aux[N]): Arbitrary[N] =
    Arbitrary(Gen.const(wit.value))

  implicit def cogenShapelessNat[N <: Nat](implicit toInt: ToInt[N]): Cogen[N] =
    Cogen[Int].contramap(_ => toInt())
}

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
package laws

import java.util.UUID

import org.scalacheck.{ Arbitrary, Gen, Cogen }
import org.scalacheck.rng.Seed
import org.scalacheck.derive.Recursive

object TestArbInstances extends ArbInstances {

  object forTestData {

    implicit val arbDefsAdt1: Arbitrary[TestTypes.adts.defs.Adt1] = {
      import org.scalacheck.Shapeless._
      derivedArbitrary(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[TestTypes.adts.defs.Adt1]
      )
    }

    implicit val cogenDefsAdt1: Cogen[TestTypes.adts.defs.Adt1] = {
      import org.scalacheck.Shapeless._
      derivedCogen(
        null : shapeless.LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkCogen[TestTypes.adts.defs.Adt1]
      )
    }

    implicit val recDefsIntList: Recursive[TestTypes.adts.recursive.IntList] =
      Recursive(Gen.const(TestTypes.adts.recursive.IntNil))

    implicit val arbDefsIntList: Arbitrary[TestTypes.adts.recursive.IntList] = {
      import org.scalacheck.Shapeless._
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
  }

  /** Inserts custom Atoms besides the built-in ones */
  implicit override def arbModelAtom: Arbitrary[Atom[_]] = Arbitrary {
    import TestInstances.atomic._
    import TestTypes.Whatever
    Gen.oneOf(
      super.arbModelAtom.arbitrary,
      Gen.oneOf[Atom[_]](Atom[UUID], Atom[Whatever.type])
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

  implicit def arbUuid(implicit al: Arbitrary[Long]): Arbitrary[UUID] =
    Arbitrary(Gen.uuid)

  implicit val cogenUuid: Cogen[UUID] =
    Cogen[String].contramap(_.toString)
}

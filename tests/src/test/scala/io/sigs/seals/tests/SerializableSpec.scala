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
package tests

import java.util.UUID
import java.time.DayOfWeek

import cats.implicits._

import core.Refinement
import laws.{ TestInstances, TestTypes, MyUUID }

// TODO: check serial version IDs

class SerializableSpec extends BaseSpec {

  "Model" - {

    import Model.Atom.atom

    "Primitives should preserve identity" in {
      checkId(Model.HNil)
      checkId(Model.CNil)
    }

    "Atoms should be serializable" in {
      import TestInstances.atomic.atomicMyUUID
      checkSer(atom[Int])
      checkSer(atom[String])
      checkSer(atom[UUID])
      checkSer(atom[MyUUID])
    }

    "Products should be serializable" in {
      checkSer('i -> atom[Int] :: 's -> atom[String] :: Model.HNil)
      checkSer('i -> atom[Int] :: Model.HCons('s, optional = true, atom[String], Model.HNil))
      checkSer(
        'x -> ('i -> atom[Int] :+: 's -> atom[String] :+: Model.CNil) :: 'y -> atom[String] :: Model.HNil
      )
    }

    "Sums should be serializable" in {
      checkSer('i -> atom[Int] :+: 's -> atom[String] :+: Model.CNil)
      checkSer(
        'p -> ('i -> atom[Int] :: Model.HNil) :+: 'q -> atom[String] :+: Model.CNil
      )
    }

    "Vector should be serializable" in {
      checkSer[Model](Model.Vector(atom[String]))
      checkSer(Reified[TestTypes.collections.Cyclic].model)
    }

    "Cyclic models should be serializable" in {
      lazy val mod1: Model.CCons = Model.CCons(
        'p,
        Model.HCons('s, atom[String], Model.HNil),
        Model.CCons(
          'q,
          Model.HCons('r, mod1, Model.HCons('s, atom[String], Model.HNil)),
          Model.CNil
        )
      )
      checkSer(mod1)
      // force evaluate thunks:
      val x = mod1.tail.asInstanceOf[Model.CCons].head.asInstanceOf[Model.HCons[Model.HCons[Model.HNil.type]]].head
      checkSer(x)
      checkSer(mod1)

      lazy val mod2: Model.HCons[Model.HCons[Model.HNil.type]] = Model.HCons(
        'm1,
        mod1,
        Model.HCons('m2, mod2, Model.HNil)
      )
      checkSer(mod2)
      // force evaluate thunks:
      val y = mod2.tail.asInstanceOf[Model.HCons[Model.HNil.type]].head
      checkSer(y)
      checkSer(mod2)
    }

    "CanBeRefined" in {
      roundtripSer(Model.CanBeRefined[Model.Atom])
      roundtripSer(Model.CanBeRefined[Model.HCons[Model.HList]])
      roundtripSer(Model.CanBeRefined[Model.HCons[Model.HNil.type]])
      roundtripSer(Model.CanBeRefined[Model.HCons[Model.HCons[Model.HNil.type]]])
      roundtripSer(Model.CanBeRefined[Model.CCons])
      roundtripSer(Model.CanBeRefined[Model.Vector])
    }

    "Refined models" in {
      checkSer(Model.CanBeRefined.atomCanBeRefined.refine(atom[Int], Refinement.enum[DayOfWeek].semantics))
      checkSer(Model.CanBeRefined.hConsCanBeRefined.refine('i -> atom[Int] :: Model.HNil, Refinement.enum[DayOfWeek].semantics))
      checkSer(Model.CanBeRefined.cConsCanBeRefined.refine('i -> atom[Int] :+: Model.CNil, Refinement.enum[DayOfWeek].semantics))
      checkSer(Model.CanBeRefined.vectorCanBeRefined.refine(Model.Vector(atom[String]), Refinement.enum[DayOfWeek].semantics))
    }
  }

  "Reified" - {

    "Atoms" in {
      roundtripSer(Reified[Int])
      roundtripSer(Reified[String])
    }

    "Derived instances" in {
      import TestTypes.adts.iso._
      roundtripSer(Reified[Adt1])
      roundtripSer(Reified[Adt1.Foo])
      roundtripSer(Reified[Adt1.Boo.type])
    }

    "Instances with non-serializable defaults" in {
      import TestTypes.custom.{ NonSer, NonSerializableDefault }
      // make sure that it's indeed non-serializable:
      a [java.io.NotSerializableException] shouldBe thrownBy {
        roundtripSer(NonSer.empty)
      }
      // we can't do anything, the Reified instance
      // won't be serializable either:
      a [java.io.NotSerializableException] shouldBe thrownBy {
        roundtripSer(Reified[NonSerializableDefault])
      }
    }

    "Refined instances" in {
      roundtripSer(Reified[Int].refined(Refinement.enum[DayOfWeek]))
    }
  }

  "Atomic" - {

    "Built-in" in {
      checkId(Atomic[Int])
      checkId(Atomic[UUID])
    }

    "Custom" in {
      import TestInstances.atomic.atomicMyUUID
      checkId(Atomic[MyUUID])
    }
  }

  "Refinement" - {

    "enum" in {
      checkSer(Refinement.enum[java.time.Month])
    }

    "Semantics" in {
      checkSer(Refinement.Semantics.greater[Int](5))
      checkSer(Refinement.Semantics.less[Int](5))
      checkSer(Refinement.Semantics(uuid"640791b5-4b33-4d0b-98a8-40a758dcf3c1", Refinement.ReprFormat.single("foo")))
    }

    "ReprFormat" in {
      checkSer(Refinement.ReprFormat.single("xyz"))
      checkSer(Refinement.ReprFormat("xyz", true, "pqr"))
    }
  }

  "Envelope" - {

    "of Atoms" in {
      checkSer(Envelope[Int](42))
      checkSer(Envelope[String]("abc"))
    }

    "of ADTs" in {
      import TestTypes.adts.iso.Adt1
      checkSer(Envelope[Adt1](Adt1.Foo("ert", 42)))
    }
  }

  "EnumLike" - {

    "for Java enums" in {
      roundtripSer(core.EnumLike[java.time.Month])
    }

    "custom" in {
      roundtripSer(core.EnumLike[EnumLikeSpec.Switch])
    }
  }

  "Compat" - {
    import TestTypes.adts.defs._

    "Products" in {
      roundtripSer(Compat[Adt1.C, Adt2.C]) shouldBe a [Compat[_, _]]
      roundtripSer(Compat[Adt1.Dummy.type, Adt2.Dummy.type]) shouldBe a [Compat[_, _]]
    }

    "Sums" in {
      roundtripSer(Compat[Adt1, Adt2]) shouldBe a [Compat[_, _]]
    }

    "Cyclic" in {
      import TestTypes.adts.recursive.{IntList, v2}
      roundtripSer(Compat[IntList, v2.IntList]) shouldBe a [Compat[_, _]]
    }
  }
}

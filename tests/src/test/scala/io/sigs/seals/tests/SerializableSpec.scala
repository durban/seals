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
      checkSer('x -> Model.Vector(atom[String]))
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
      val x = mod1.tail.asInstanceOf[Model.CCons].head.asInstanceOf[Model.HCons].head
      checkSer(x)
      checkSer(mod1)

      lazy val mod2: Model.HCons = Model.HCons(
        'm1,
        mod1,
        Model.HCons('m2, mod2, Model.HNil)
      )
      checkSer(mod2)
      // force evaluate thunks:
      val y = mod2.tail.asInstanceOf[Model.HCons].head
      checkSer(y)
      checkSer(mod2)
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

  "AtomRegistry" - {

    import Model.Atom.atom

    "built-in" in {
      checkSer[AtomRegistry](AtomRegistry.builtinAtomRegistry)
    }

    "of model" in {
      import TestTypes.adts.iso._
      checkSer[AtomRegistry](Reified[Adt1].model.atomRegistry)
    }

    "custom" in {
      checkSer[AtomRegistry](TestInstances.atomic.registry)
      checkSer[AtomRegistry](AtomRegistry.fromMap(Map.empty))
      checkSer[AtomRegistry](AtomRegistry.fromMap(Map(UUID.randomUUID() -> atom[Int])))
      checkSer[AtomRegistry](AtomRegistry.fromMap(Map(
        UUID.randomUUID() -> atom[Int],
        UUID.randomUUID() -> atom[Int],
        UUID.randomUUID() -> atom[Int],
        UUID.randomUUID() -> atom[Int],
        UUID.randomUUID() -> atom[Int]
      )))
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

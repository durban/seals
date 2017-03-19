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

import shapeless._
import shapeless.record._
import shapeless.union._

import scodec.bits._

import core.Refinement
import laws.{ TestInstances, TestTypes }

class ReifiedSpec extends BaseSpec {

  import Model.Atom.atom

  "Reified" - {

    "for Records" - {

      "should exist" in {
        Reified[HNil].model should === (Model.HNil)
        Reified[Record.`'p -> Int`.T].model should === ('p -> atom[Int] :: Model.HNil)
        Reified[Record.`'s -> String, 'i -> Int`.T].model should === (
          's -> atom[String] :: 'i -> atom[Int] :: Model.HNil
        )
      }
    }

    "for Unions" - {

      "should exist" in {
        Reified[CNil].model should === (Model.CNil)
        Reified[Union.`'i -> Int`.T].model should === ('i -> atom[Int] :+: Model.CNil)
        Reified[Union.`'s -> String, 'i -> Int`.T].model should === (
          's -> atom[String] :+: 'i -> atom[Int] :+: Model.CNil
        )
      }
    }

    "derived with Generic" - {

      "for simple ADTs" - {

        "existence" -{
          import TestTypes.adts.iso.Adt1

          "should exist" in {
            Reified[Adt1.Foo].model should === (Adt1.Foo.expModel)
            Reified[Adt1].model should === (Adt1.expModel)
          }

          "should work with case objects" in {
            Reified[Adt1.Boo.type].model should === (Model.HNil)
          }

          "should work with Option" in {
            Reified[Option[Int]].model should === (
              'None -> (Model.HNil) :+: 'Some -> ('value -> atom[Int] :: Model.HNil) :+: Model.CNil
            )
          }
        }

        "should handle optional fields correctly" in {
          import TestTypes.adts.defs._
          checkCompatible(Reified[Adt1.C].model, Reified[Adt2.C].model)
          checkCompatible(Reified[Adt1].model, Reified[Adt2].model)
        }

        "should work with Atomic" in {
          import TestTypes.custom.WithUuid
          import TestInstances.atomic.atomicMyUUID
          Reified[WithUuid].model should === (WithUuid.expModel)
        }

        import TestTypes.adts.rename._

        val t1 = Reified[C1]
        val tr = Reified[C2]
        val u1 = Reified[v1.Adt]
        val ur = Reified[v2.Adt]

        "should have a correct (identity) equals + hashCode" in {
          checkEqHash(t1, t1)
          checkEqHash(tr, tr)
          checkEqHash(u1, u1)
          checkEqHash(ur, ur)
          checkNotEqHash(t1, tr)
          checkNotEqHash(t1, u1)
          checkNotEqHash(t1, ur)
          checkNotEqHash(u1, ur)
          checkNotEqHash(u1, t1)
          checkNotEqHash(u1, tr)
        }
      }

      "for recursive ADTs" - {
        import TestTypes.adts.recursive._

        val il1 = Reified[IntList]
        val mr1 = Reified[MutRec1]

        "should exist" in {
          Reified[IntList].model should === (IntList.expModel)
        }

        "should exist for various cycles" in {
          List[Model](
            Reified[IntList].model,
            Reified[MutRec1].model,
            Reified[MutRec2].model,
            Reified[Deep1].model,
            Reified[Deep2].model,
            Reified[Deep3].model,
            Reified[Deep4].model
          )
        }

        "should have a correct (identity) equals + hashCode" in {
          checkEqHash(il1, il1)
          checkEqHash(mr1, mr1)
          checkNotEqHash(il1, mr1)
          checkNotEqHash(mr1, il1)
        }
      }

      "for fields with defaults" - {
        import TestTypes.adts.defs.Adt2

        "in case classes" in {
          Reified[Adt2.C].model should === (Adt2.C.expModel)
        }

        "in ADTs" in {
          Reified[Adt2].model should === (Adt2.expModel)
        }
      }
    }

    "derived for collections" - {

      import TestTypes.collections._

      "List" in {
        Reified[List[Boolean]].model should === (Model.Vector(atom[Boolean]))
        Reified[WithList].model should === (WithList.expModel)
      }

      "Vector" in {
        Reified[Vector[Boolean]].model should === (Model.Vector(atom[Boolean]))
        Reified[WithVector].model should === (WithVector.expModel)
      }

      "Inside ADTs" in {
        Reified[Adt].model should === (Adt.expModel)
      }
    }

    "cached in companion object" - {

      import TestTypes.caching._

      "sealed trait" in {
        val inst = implicitly[Reified[STr]]
        inst shouldBe theSameInstanceAs (STr.r)
        inst.model should === (STr.expModel)
      }

      "case class" in {
        val inst = implicitly[Reified[Jack]]
        inst shouldBe theSameInstanceAs (Jack.r)
        inst.model should === (Jack.expModel)
      }

      "recursive ADT" in {
        val inst = implicitly[Reified[StrList]]
        inst shouldBe theSameInstanceAs (StrList.r)
        inst.model should === (StrList.expModel)
      }
    }
  }

  "Reified.toString" - {

    sealed trait Adt
    final case class IS(i: Int, s: String) extends Adt
    final case class IX(i: Int) extends Adt

    val hl = Reified[Record.`'i -> Int, 's -> String`.T]
    val cc = Reified[IS]
    type H1 = Record.`'i -> Int, 's -> String`.T
    type H2 = Record.`'i -> Int`.T
    val cp = Reified[Union.`'IS -> H1, 'IX -> H2`.T]
    val st = Reified[Adt]

    "toString should show the Model" in {
      hl.toString should === ("Reified['i -> Int :: 's -> String :: HNil]")
      cc.toString should === (hl.toString)
      cp.toString should === ("Reified['IS -> ('i -> Int :: 's -> String :: HNil) :+: 'IX -> ('i -> Int :: HNil) :+: CNil]")
      st.toString should === (cp.toString)
    }
  }

  "Reified.imap" - {

    import TestInstances.reified._

    final case class C(i: Int, u: UUID)
    final case class D(i: Int, u: (Long, Long))

    "Instance embedded in ADT" in {
      val rC = Reified[C]
      val rD = Reified[D]
      rC.model should === (rD.model)
    }
  }

  "Refinements" - {

    "Atoms" in {
      val int = Reified[Int]
      val r1 = int.refined(Refinement.enum[MyTestEnumWithArgs]) // 2 elements
      val r2 = int.refined(Refinement.enum[MyTestEnumWithToString]) // 2 elements
      val r3 = int.refined(Refinement.enum[MyTestEnum]) // 3 elements
      assert(!int.model.compatible(r1.model))
      assert(!int.model.compatible(r2.model))
      assert(!int.model.compatible(r3.model))
      assert(!r1.model.compatible(int.model))
      assert(!r2.model.compatible(int.model))
      assert(!r3.model.compatible(int.model))
      assert(!r1.model.compatible(r3.model))
      assert(!r2.model.compatible(r3.model))
      assert(!r3.model.compatible(r1.model))
      assert(!r3.model.compatible(r2.model))
      assert(r1.model.compatible(r2.model))
      assert(r2.model.compatible(r1.model))
    }

    "Successive refinements" in {
      val r0 = Reified[Int]
      val r1 = r0.refined(Refinement.enum[MyTestEnum]) // 3 elements
      val r2 = r1.refined(ReifiedSpec.refineEnumToBool)
      // sanity-check types:
      locally { r1 : Reified[MyTestEnum] }
      locally { r2 : Reified[Boolean] }
      // check models:
      assert(!r0.model.compatible(r1.model))
      assert(!r0.model.compatible(r2.model))
      assert(!r1.model.compatible(r2.model))
    }

    "Descriptions" in {
      val r0 = Reified[Int]
      val r1 = r0.refined(Refinement.enum[MyTestEnum])
      r1.toString should === ("Reified[0 ≤ Int ≤ 2]")

      val r3 = r0.refined(new Refinement[Boolean] {
        override type Repr = Int
        override val uuid = uuid"c935143b-64fe-4c2b-8912-09af5a88c734"
        override def desc(r: String) = s"Flag(${r})"
        override def from(i: Int) = i match {
          case 0 => Right(false)
          case 1 => Right(true)
          case _ => Left("bad")
        }
        override def to(b: Boolean) = if (b) 1 else 0
      })
      r3.toString should === ("Reified[Flag(Int)]")
    }

    "Refining with UUID only" in {
      val r0 = Reified[Int]
      val r1 = r0.pimap[Int](uuid"6445fb0b-1814-4e2d-bd8b-750f6e906a28") { i =>
        if ((i >= 0) && (i <= 15)) Right(i)
        else Left("out of range")
      } { i => i }
      r1.model.compatible(r0.model) should === (false)
      r1.toString should === ("Reified[Int{?}]")
    }

    "shapeless.Nat" in {
      val r0 = Reified[Nat._0]
      val r2a = Reified[Nat._2]
      val r2b = Reified[Succ[Nat._1]]
      val r9 = Reified[Nat._9]
      checkNotEqHashCompat(r0.model, Reified[Int].model)
      checkNotEqHashCompat(r0.model, r2a.model)
      checkNotEqHashCompat(r0.model, r2b.model)
      checkNotEqHashCompat(r0.model, r9.model)
      checkNotEqHashCompat(r2a.model, r9.model)
      checkEqHashCompat(r2a.model, r2b.model)
    }
  }
}

object ReifiedSpec {

  val refineEnumToBool: Refinement.Aux[Boolean, MyTestEnum] = new Refinement[Boolean] {
    override type Repr = MyTestEnum
    override val uuid = (Refinement.root / hex"deadbeef").uuid
    override def from(e: MyTestEnum) = e match {
      case MyTestEnum.FOO => Right(true)
      case MyTestEnum.BAR => Right(false)
      case MyTestEnum.BAZ => Left("error")
    }
    override def to(a: Boolean) = if (a) {
      MyTestEnum.FOO
    } else {
      MyTestEnum.BAR
    }
  }
}

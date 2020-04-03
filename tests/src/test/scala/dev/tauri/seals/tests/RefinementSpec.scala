/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.Show

import core.Refinement

class RefinementSpec extends BaseSpec {

  import RefinementSpec._

  "Refinement" - {

    "equality is based on UUID" in {
      val u = uuid"ceea8d1a-e631-4ad6-afb4-404f2508e61b"
      val r1 = new Refinement[Boolean] {
        override type Repr = Int
        override val uuid = (root / u).uuid
        override def repr = Refinement.ReprFormat.single("Bool1")
        def from(idx: Int) = idx match {
          case 0 => Right(false)
          case 1 => Right(true)
          case _ => Left("boo")
        }
        def to(a: Boolean) = if (a) 1 else 0
      }
      val r2 = new Refinement[Boolean] {
        override type Repr = Int
        override val uuid = (root / u).uuid
        override def repr = Refinement.ReprFormat.single("Bool2")
        def from(idx: Int) = idx match {
          case 10 => Right(false)
          case 11 => Right(true)
          case _ => Left("boo")
        }
        def to(a: Boolean) = if (a) 11 else 10
      }
      val r3 = new Refinement[Boolean] {
        override type Repr = Int
        override val uuid = (root / uuid"d9d828cc-c1f5-43e7-91f7-011c71559bc1").uuid
        override def repr = Refinement.ReprFormat.single("Bool3")
        def from(idx: Int) = idx match {
          case 10 => Right(false)
          case 11 => Right(true)
          case _ => Left("boo")
        }
        def to(a: Boolean) = if (a) 11 else 10
      }
      checkEqHash(r1, r2)
      checkNotEqHash(r1, r3)
      checkNotEqHash(r2, r3)
    }
  }

  "Refinement.Semantics" - {

    "greater and less" in {
      val g1a = Refinement.Semantics.greater[Foo](Foo(5, "ert"))
      val g1b = Refinement.Semantics.greater[Foo](Foo(5, "ert"))
      val g2 = Refinement.Semantics.greater[Foo](Foo(6, "ert"))
      val l1a = Refinement.Semantics.less[Foo](Foo(5, "ert"))
      val l1b = Refinement.Semantics.less[Foo](Foo(5, "ert"))
      val l2 = Refinement.Semantics.less[Foo](Foo(6, "ert"))

      checkEqHash(g1a, g1b)
      checkEqHash(l1a, l1b)
      checkNotEqHash(g1a, g2)
      checkNotEqHash(l1a, l2)
      checkNotEqHash(g1a, l1a)
      checkNotEqHash(g2, l2)

      val tricky1 = Refinement.Semantics.greater[Foo2](Foo2(5, "ert"))
      val tricky2 = Refinement.Semantics.less[Foo2](Foo2(5, "ert"))

      checkEqHash(g1a, tricky1)
      checkEqHash(l1a, tricky2)
      checkNotEqHash(tricky1, l1a)
      checkNotEqHash(tricky1, l2)
      checkNotEqHash(tricky2, g1a)
      checkNotEqHash(tricky2, g2)
      checkNotEqHash(tricky1, tricky2)
    }

    "set/map" in {
      checkEqHash(Refinement.Semantics.set, Refinement.Semantics.set)
      checkEqHash(Refinement.Semantics.map, Refinement.Semantics.map)
      checkNotEqHash(Refinement.Semantics.set, Refinement.Semantics.map)
      checkNotEqHash(Refinement.Semantics.set, Refinement.Semantics.greater[Foo](Foo(5, "ert")))
      checkNotEqHash(Refinement.Semantics.map, Refinement.Semantics.greater[Foo](Foo(5, "ert")))
      Refinement.Semantics.set.repr.repr("?") should === ("set{?}")
      Refinement.Semantics.map.repr.repr("?") should === ("map{?}")
    }
  }

  "Refinement.ReprFormat" - {

    import Refinement.ReprFormat

    "ctor" in {
      val rf1 = ReprFormat("XXX", true, "YYY")
      rf1.repr("ZZZ") should === ("XXXZZZYYY")
      val rf2 = ReprFormat("XXX", false, "YYY")
      rf2.repr("ZZZ") should === ("XXXYYY")
    }

    "single" in {
      val rf = ReprFormat.single("ABC")
      rf.repr("XYZ") should === ("ABC")
    }

    "combine" - {

      "simple" in {
        val rf1 = ReprFormat("[", true, "]")
        val rf2 = ReprFormat("<", true, ">")
        (rf1 combine rf2).repr("x") should === ("<[x]>")
        (rf2 combine rf1).repr("x") should === ("[<x>]")
      }

      "single" in {
        val rf1 = ReprFormat("[", true, "]")
        val rf2 = ReprFormat.single("!")
        (rf1 combine rf2).repr("x") should === ("!")
        (rf2 combine rf1).repr("x") should === ("[!]")
        val rf3 = ReprFormat.single("-")
        (rf1 combine rf3).repr("x") should === ("-")
        (rf2 combine rf3).repr("x") should === ("-")
      }

      "multiple combine" in {
        val rf1 = ReprFormat("[", true, "]")
        val rf2 = ReprFormat("<", true, ">")
        val rf3 = ReprFormat("(", true, ")")
        (rf1 combine rf2 combine rf3).repr("x") should === ("(<[x]>)")
        (rf3 combine rf2 combine rf1).repr("x") should === ("[<(x)>]")
        val rf4 = ReprFormat.single("!")
        (rf1 combine rf2 combine rf4).repr("x") should === ("!")
        (rf1 combine rf4 combine rf3).repr("x") should === ("(!)")
        (rf4 combine rf2 combine rf3).repr("x") should === ("(<!>)")
      }
    }
  }
}

object RefinementSpec {

  final case class Foo(i: Int, s: String)
  object Foo {
    implicit val sh: Show[Foo] =
      Show.fromToString
    implicit val rei: Reified[Foo] =
      shapeless.cachedImplicit
  }

  final case class Foo2(i: Int, s: String)
  object Foo2 {
    implicit val sh: Show[Foo2] =
      Show.fromToString
    implicit val rei: Reified[Foo2] =
      shapeless.cachedImplicit
  }
}

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
package core

import scala.annotation.tailrec
import scala.util.Random
import org.scalacheck.{ Arbitrary, Gen }
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class IdentitySetSpec extends tests.BaseSpec with ScalaCheckDrivenPropertyChecks {

  import IdentitySetSpec._

  private val e = IdentitySet.empty[Cls]

  "Basic functionality" - {

    "containment" in {
      e.contains(cls()) should === (false)
      e should have size (0)

      val c = cls()
      val s = e + c
      s.contains(c) should === (true)
      s.contains(cls()) should === (false)
      s should have size (1)
    }

    "removal" in {
      val c = cls()
      val s = e + cls() + cls() + c + cls() + cls()
      s.contains(c) should === (true)
      s should have size (5)

      val r = s - c
      r.contains(c) should === (false)
      r should have size (4)
    }

    "no duplicates" in {
      val c = cls()
      val s = e + c + cls() + c + cls()
      s.contains(c) should === (true)
      s should have size (3)
      s.toList.filter(_ eq c) should === (c :: Nil)
    }

    "union" in {
      val (c1, c2, c3) = (cls(), cls(), cls())
      val (c4, c5, c6) = (cls(), cls(), cls())
      val cs = List(c1, c2, c3, c4, c5, c6)
      val a = e + c1 + c2 + c3
      val b = e + c4 + c5 + c6
      val u = a union b
      u should have size (6)
      val l = u.toList
      l should have size (6)
      l.forall { c =>
        cs.exists(_ eq c)
      }
    }

    "intersection" in {
      val (c1, c2) = (cls(), cls())
      val (c3, c4) = (cls(), cls())

      val a = e + c1 + c2 + c3
      val b = e + c2 + c3 + c4 + cls()

      a filter b
      val i = a intersect b
      i should have size (2)
      i should === (e + c2 + c3)
    }

    "equality" in {
      val (c1, c2, c3) = (cls(), cls(), cls())
      val a = e + c1 + c2 + c3
      val b = e + c3 + c1 + c2
      val c = e + cls() + c3 + cls()

      e should === (e)
      e should !== (a)
      e should !== (b)
      e should !== (c)

      a should === (a)
      a should === (b)
      a should !== (e)
      a should !== (c)

      b should === (b)
      b should === (a)
      b should !== (e)
      b should !== (c)

      c should === (c)
      c should !== (a)
      c should !== (b)
      c should !== (e)
    }
  }

  "Many elements" - {

    val bigSet = Stream.continually(cls()).take(10000).foldLeft(e)(_ + _)
    val x = cls()

    "iterating" in {
      for (c <- bigSet) {
        bigSet(c) should === (true)
        bigSet(x) should === (false)
      }
    }

    "removing" in {
      val someItems = bigSet.foldLeft[List[Cls]](Nil) { (l, c) =>
        if (Random.nextInt(100) == 0) {
          c :: l
        } else {
          l
        }
      }
      someItems.foldLeft(bigSet) { (s, i) =>
        val newSet = s - i
        newSet(i) should === (false)
        newSet.subsetOf(bigSet) should === (true)
        newSet
      }
    }

    "union" in {
      val bigSet2 = Stream.continually(cls()).take(10000).foldLeft(e)(_ + _)
      val u = bigSet union bigSet2
      u.size should === (bigSet.size + bigSet2.size)
      bigSet.subsetOf(u) should === (true)
      bigSet2.subsetOf(u) should === (true)
    }

    "intersection" in {
      val i = bigSet intersect (e + cls() + bigSet.iterator.next())
      i should have size (1)
    }

    "equality" in {
      val bigSet2 = bigSet.map(identity)
      bigSet2 shouldNot be theSameInstanceAs(bigSet)
      bigSet2 should === (bigSet)
    }
  }

  "Laws" - {

    implicit def arbIdentitySet(implicit arbCls: Arbitrary[Cls]): Arbitrary[IdentitySet[Cls]] = {
      Arbitrary {
        Gen.listOf(arbCls.arbitrary).map(
          _.foldLeft(IdentitySet.empty[Cls])(_ + _)
        )
      }
    }

    def genSetAndMissingItem(implicit arbCls: Arbitrary[Cls]): Gen[(IdentitySet[Cls], Cls)] = for {
      len <- Gen.choose(1, 10)
      lst <- Gen.listOfN(len, arbCls.arbitrary)
      idx <- Gen.choose(0, len - 1)
    } yield {
      val item = lst(idx)
      val rls = lst.filterNot(cls => cls eq item)
      (rls.foldLeft(IdentitySet.empty[Cls])(_ + _), item)
    }

    def genSetAndIncludedItem(implicit arbCls: Arbitrary[Cls]): Gen[(IdentitySet[Cls], Cls)] = for {
      len <- Gen.choose(1, 10)
      lst <- Gen.listOfN(len, arbCls.arbitrary)
      idx <- Gen.choose(0, len - 1)
    } yield (lst.foldLeft(IdentitySet.empty[Cls])(_ + _), lst(idx))

    "insert" - {

      "missing" in {
        forAll(genSetAndMissingItem) { case (s, c) =>
          whenever(!(s contains c)) {
            val sum = s + c
            sum should !== (s)
            sum should have size (s.size + 1L)
            sum.contains(c) should === (true)
            val sum2 = sum + c
            sum2 should === (sum)
          }
        }
      }

      "included" in {
        forAll(genSetAndIncludedItem) { case (s, c) =>
          whenever(s contains c) {
            val sum = s + c
            sum should === (s)
            sum should have size (s.size.toLong)
            sum shouldBe theSameInstanceAs (s)
          }
        }
      }
    }

    "remove" - {

      "included" in {
        forAll(genSetAndIncludedItem) { case (s, c) =>
          whenever(s contains c) {
            val rem = s - c
            rem should !== (s)
            rem.contains(c) should === (false)
            rem.size should === (s.size - 1)
            val rem2 = s - c
            rem2 should === (rem)
          }
        }
      }

      "missing" in {
        forAll(genSetAndMissingItem) { case (s, c) =>
          whenever(!(s contains c)) {
            val rem = s - c
            rem should === (s)
            rem shouldBe theSameInstanceAs (s)
          }
        }
      }
    }

    "iterator" in {
      forAll { (s: IdentitySet[Cls]) =>
        val lst = s.toList
        lst.size should === (s.size)
        for (e <- lst) {
          (s contains e) should === (true)
        }
        @tailrec
        def checkDuplicates(lst: List[Cls]): Unit = lst match {
          case Nil => ()
          case h :: t =>
            for (e <- t) {
              if (e eq h) fail("duplicate")
            }
            checkDuplicates(t)
        }
        checkDuplicates(lst)
      }
    }

    "union" in {
      forAll { (a: IdentitySet[Cls], b: IdentitySet[Cls]) =>
        val u = a union b
        val u2 = b union a
        u should === (u2)
        a.subsetOf(u) should === (true)
        b.subsetOf(u) should === (true)
        u.size should be <= (a.size + b.size)
        u.size should be >= (a.size max b.size)
      }
    }

    "intersection" in {
      forAll { (a: IdentitySet[Cls], b: IdentitySet[Cls]) =>
        val i = a intersect b
        val i2 = b intersect a
        i should === (i2)
        i.subsetOf(a) should === (true)
        i.subsetOf(b) should === (true)
        i.size should be <= (a.size min b.size)
        i.size should be >= 0
      }
    }
  }
}

object IdentitySetSpec {

  /**
   * Special type to verify that
   * neither `hashCode`, nor `equals`
   * is called.
   */
  final class Cls {

    final override def equals(that: Any): Boolean =
      throw new RuntimeException("equals called")

    final override def hashCode: Int =
      throw new RuntimeException("hashCode called")

    final override def toString: String =
      "Cls instance"
  }

  object Cls {

    implicit val arbCls: Arbitrary[Cls] = Arbitrary(
      Gen.oneOf(
        // fresh one:
        Gen.delay(Gen.const(cls())),
        // existing one:
        Gen.choose(0, existingInstances.size - 1).map(i => existingInstances(i))
      )
    )

    private val existingInstances: Vector[Cls] =
      Vector.fill(10)(cls())
  }

  def cls(): Cls =
    new Cls
}

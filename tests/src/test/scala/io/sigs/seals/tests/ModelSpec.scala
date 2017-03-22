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

import java.time.{ Month, DayOfWeek }

import scala.util.hashing.MurmurHash3

import shapeless.test.illTyped

import core.Refinement
import laws.MyUUID
import laws.TestInstances.atomic.atomicMyUUID

class ModelSpec extends BaseSpec {

  import Model.Atom.atom

  val l = 'l
  val m = 'm
  val n = 'n
  val p = 'p
  val q = 'q
  val r = 'r

  val a1a = atom[Int]
  val a1b = atom[Int]
  val a2 = atom[String]
  val ac = atom[MyUUID]
  val refinedAtom1 = Model.CanBeRefined[Model.Atom].refine(
    a1a,
    Refinement.enum[Month]
  )
  val refinedAtom1b = Model.CanBeRefined[Model.Atom].refine(
    a1b,
    Refinement.enum[Month]
  )
  val refinedAtom2 = Model.CanBeRefined[Model.Atom].refine(
    a1a,
    Refinement.enum[DayOfWeek]
  )

  val p1a = l -> a2 :: m -> a1b :: Model.HNil
  val p1b = l -> atom[String] :: m -> a1a :: Model.HNil
  val p2 = n -> a1a :: p -> a1b :: Model.HNil
  val p2plus = n -> a1a :: p -> a1b :: Model.HCons('x, optional = true, a2, Model.HNil)
  val p3 = p -> a2 :: q -> a2 :: r -> a1a :: Model.HNil
  val p3o = p -> a2 :: q -> a2 :: Model.HCons(r, optional = true, a1a, Model.HNil)
  val p3oMinus = p -> a2 :: q -> a2 :: Model.HNil

  val c1a = l -> ac :+: m -> a2 :+: n -> a2 :+: Model.CNil
  val c1b = Model.CCons(l, ac, c1a.tail)
  val c2 = Model.CCons(p, a2, c1a.tail)
  val c3 = p -> a2 :+: q -> a1a :+: r -> a2 :+: Model.CNil

  val pc1a = l -> p1a :+: m -> p2 :+: n -> c1a :+: Model.CNil
  val pc1b = l -> p1b :+: m -> p2 :+: n -> c1b :+: Model.CNil
  val pc2a = p -> c1a :: Model.HNil
  val pc2b = p -> c1b :: Model.HNil

  val k1a = Model.Vector(a1a)
  val k1b = Model.Vector(a1b)
  val k2a = Model.Vector(pc1a)
  val k2b = Model.Vector(pc1b)
  val k3a = 'x -> Model.Vector(p3o) :: Model.HNil
  val k3aMinus = 'x -> Model.Vector(p3oMinus) :: Model.HNil

  lazy val cy1a: Model.CCons = Model.CCons(
    l,
    Model.HCons(p, a2, Model.HNil),
    Model.CCons(
      m,
      Model.HCons(q, cy1a, Model.HCons(r, a2, Model.HNil)),
      Model.CNil
    )
  )
  lazy val cy1b: Model.CCons = Model.CCons(
    l,
    Model.HCons(p, a2, Model.HNil),
    Model.CCons(
      m,
      Model.HCons(q, cy1a, Model.HCons(r, a2, Model.HNil)),
      Model.CNil
    )
  )
  lazy val cy2a: Model.HCons[Model.HCons[Model.HNil.type]] = Model.HCons(
    p,
    cy1a,
    Model.HCons(l, ac, Model.HNil)
  )
  lazy val cy2b: Model.HCons[Model.HCons[Model.HNil.type]] = Model.HCons(
    p,
    cy1b,
    Model.HCons(l, atom[MyUUID], Model.HNil)
  )

  "equals + hashCode + compatibility" - {

    "atom" in {
      checkEqHashCompat(a1a, a1a)
      checkEqHashCompat(a1a, a1b)
      checkEqHashCompat(a1b, a1a)
      checkEqHashCompat(a2, a2)
      checkEqHashCompat(ac, ac)
      checkNotEqHashCompat(a1a, a2)
      checkNotEqHashCompat(a2, a1a)
      checkNotEqHashCompat(a2, ac)
      checkNotEqHashCompat(ac, a2)
    }

    "primitives" in {
      checkEqHashCompat(Model.HNil, Model.HNil)
      checkNotEqHashCompat(Model.HNil, Model.CNil)
      checkNotEqHashCompat(Model.HNil, a2)
      checkNotEqHashCompat(Model.HNil, ac)
      checkEqHashCompat(Model.CNil, Model.CNil)
      checkNotEqHashCompat(Model.CNil, Model.HNil)
      checkNotEqHashCompat(Model.CNil, a2)
      checkNotEqHashCompat(Model.CNil, ac)
    }

    "simple product" in {
      checkEqHashCompat(p1a, p1a)
      checkEqHashCompat(p1a, p1b)
      checkEqHashCompat(p1b, p1b)
      checkEqHashCompat(p1b, p1a)
      checkEqHashCompat(p2, p2)
      checkEqHashCompat(p3, p3)
      checkEqHashCompat(p3o, p3o)
      checkNotEqHashCompat(p2, p1a)
      checkNotEqHashCompat(p2, p1b)
      checkNotEqHashCompat(p2, p3)
      checkNotEqHashCompat(p2, p3o)
      checkNotEqHash(p2, p2plus)
      checkNotEqHashCompat(p1a, p2)
      checkNotEqHashCompat(p1a, p3)
      checkNotEqHashCompat(p1a, p3o)
      checkNotEqHashCompat(p1b, p2)
      checkNotEqHashCompat(p1b, p3)
      checkNotEqHashCompat(p1b, p3o)
      checkNotEqHashCompat(p3, p2)
      checkNotEqHashCompat(p3, p1a)
      checkNotEqHashCompat(p3, p1b)
      checkNotEqHashCompat(p3, p3o)
      checkNotEqHashCompat(p3o, p3)
      checkNotEqHash(p3o, p3oMinus)
    }

    "simple sum" in {
      checkEqHashCompat(c1a, c1a)
      checkEqHashCompat(c1a, c1b)
      checkEqHashCompat(c2, c2)
      checkEqHashCompat(c3, c3)
      checkNotEqHashCompat(c1a, c2)
      checkNotEqHashCompat(c1a, c3)
      checkNotEqHashCompat(c1b, c2)
      checkNotEqHashCompat(c1b, c3)
      checkNotEqHashCompat(c2, c1a)
      checkNotEqHashCompat(c2, c1b)
      checkNotEqHashCompat(c2, c3)
      checkNotEqHashCompat(c3, c1a)
      checkNotEqHashCompat(c3, c1b)
      checkNotEqHashCompat(c3, c2)
    }

    "sum and product" in {
      for (p <- Seq(p1a, p1b, p2, p3, p3o)) {
        for (c <- Seq(c1a, c1b, c2, c3)) {
          checkNotEqHashCompat(p, c)
        }
      }

      checkEqHashCompat(pc1a, pc1a)
      checkEqHashCompat(pc1a, pc1b)
      checkNotEqHashCompat(pc1a, pc2a)
      checkNotEqHashCompat(pc1a, pc2b)
      checkEqHashCompat(pc1b, pc1b)
      checkEqHashCompat(pc1b, pc1a)
      checkNotEqHashCompat(pc1b, pc2a)
      checkNotEqHashCompat(pc1b, pc2b)
      checkEqHashCompat(pc2a, pc2a)
      checkEqHashCompat(pc2a, pc2b)
      checkNotEqHashCompat(pc2a, pc1a)
      checkNotEqHashCompat(pc2a, pc1b)
      checkEqHashCompat(pc2b, pc2b)
      checkEqHashCompat(pc2b, pc2a)
      checkNotEqHashCompat(pc2b, pc1a)
      checkNotEqHashCompat(pc2b, pc1b)
    }

    "vector" in {
      checkEqHashCompat(k1a, k1a)
      checkEqHashCompat(k1a, k1b)
      checkEqHashCompat(k2a, k2a)
      checkEqHashCompat(k2a, k2b)
      checkNotEqHashCompat(k1a, k2a)
      checkNotEqHashCompat(k2a, k1a)
      checkNotEqHashCompat(k1a, a1a)
      checkNotEqHashCompat(k2a, pc1a)
    }

    "cyclic model" in {
      checkEqHashCompat(cy1a, cy1a)
      checkEqHashCompat(cy1a, cy1b)
      checkEqHashCompat(cy1b, cy1b)
      checkEqHashCompat(cy1b, cy1a)
      checkNotEqHashCompat(cy1a, cy2a)
      checkNotEqHashCompat(cy1a, cy2b)
      checkNotEqHashCompat(cy1b, cy2a)
      checkNotEqHashCompat(cy1b, cy2b)
      checkEqHashCompat(cy2a, cy2a)
      checkEqHashCompat(cy2a, cy2b)
      checkEqHashCompat(cy2b, cy2b)
      checkEqHashCompat(cy2b, cy2a)
      checkNotEqHashCompat(cy2a, cy1a)
      checkNotEqHashCompat(cy2a, cy1b)
      checkNotEqHashCompat(cy2b, cy1a)
      checkNotEqHashCompat(cy2b, cy1b)
    }

    "order should matter in HList/Coproduct" in {
      val x = p -> a2 :: q -> a1b :: Model.HNil
      val y = p -> a1b :: q -> a2 :: Model.HNil
      checkNotEqHashCompat(x, y)
      val z = p -> a2 :+: q -> a1b :+: Model.CNil
      val w = p -> a1b :+: q -> a2 :+: Model.CNil
      checkNotEqHashCompat(z, w)

      checkNotEqHashCompat(x, z)
      checkNotEqHashCompat(y, w)
    }

    "amount of atoms should matter" in {
      val x = l -> a2 :: Model.HNil
      val y = l -> a2 :: m -> a2 :: Model.HNil
      checkNotEqHashCompat(x, y)
      val p = l -> a2 :+: Model.CNil
      val q = l -> a2 :+: l -> a2 :+: Model.CNil
      checkNotEqHashCompat(p, q)

      checkNotEqHashCompat(x, p)
      checkNotEqHashCompat(y, q)
    }

    "labels should matter" in {
      val x1 = 'p -> a2 :: 'q -> ac :: Model.HNil
      val x2 = 'p -> a2 :: 'q -> ac :: Model.HNil
      val y = 'p -> a2 :: 'x -> ac :: Model.HNil
      checkEqHashCompat(x1, x2)
      checkEqHashCompat(x2, x1)
      checkNotEqHashCompat(x1, y)
      checkNotEqHashCompat(y, x1)
      val p1 = 'p -> a2 :+: 'q -> ac :+: Model.CNil
      val p2 = 'p -> a2 :+: 'q -> ac :+: Model.CNil
      val q = 'p -> a2 :+: 'x -> ac :+: Model.CNil
      checkEqHashCompat(p1, p2)
      checkEqHashCompat(p2, p1)
      checkNotEqHashCompat(p1, q)
      checkNotEqHashCompat(q, p1)
    }

    "refinement should matter" in {
      checkNotEqHashCompat(a1a, refinedAtom1)
      checkNotEqHashCompat(a1a, refinedAtom2)
      checkNotEqHashCompat(refinedAtom1, refinedAtom2)
      checkEqHashCompat(refinedAtom1, refinedAtom1b)
    }
  }

  "compatible" in {
    checkCompatible(p2, p2plus)
    checkCompatible(p3o, p3oMinus)
    checkCompatible(k3a, k3aMinus)
  }

  "toString/desc" in {
    p1a.desc should === ("'l -> String :: 'm -> Int :: HNil")
    p1a.toString should === ("Model['l -> String :: 'm -> Int :: HNil]")

    c1a.desc should === ("'l -> MyUUID :+: 'm -> String :+: 'n -> String :+: CNil")

    val pc1aExp =
      "'l -> ('l -> String :: 'm -> Int :: HNil) :+: " +
      "'m -> ('n -> Int :: 'p -> Int :: HNil) :+: " +
      "'n -> ('l -> MyUUID :+: 'm -> String :+: 'n -> String :+: CNil) :+: CNil"
    pc1a.desc should === (pc1aExp)

    cy1a.desc should === (
      "'l -> ('p -> String :: HNil) :+: 'm -> ('q -> <...> :: 'r -> String :: HNil) :+: CNil"
    )

    p3o.desc should === (
      "'p -> String :: 'q -> String :: 'r -> Int? :: HNil"
    )

    (('p, ac) :: Model.HNil).desc should === ("'p -> MyUUID :: HNil")

    k1a.desc should === ("Int*")
    k2a.desc should === (s"(${pc1aExp})*")

    refinedAtom1.desc should === ("0 ≤ Int ≤ 11")
    refinedAtom1.toString should === ("Model[0 ≤ Int ≤ 11]")
  }

  "cats.Eq" in {
    val e = cats.Eq[Model]
    assert(e.eqv(atom[String], a2))
    assert(e.eqv(atom[MyUUID], ac))
    assert(e.eqv(k1a, k1b))
  }

  "illegal structures" in {
    illTyped("Model.HCons('a, Atom[Int], Atom[String])")
    illTyped("Model.HCons('a, Atom[Int], Model.CNil)")
    illTyped("Model.HCons('a, Atom[Int], Model.CCons('b, Atom[String], Model.CNil))")

    illTyped("Model.CCons('a, Atom[Int], Atom[String])")
    illTyped("Model.CCons('a, Atom[Int], Model.HNil)")
    illTyped("Model.CCons('a, Atom[Int], Model.HCons('b, Atom[String], Model.HNil))")
  }

  "fold" - {

    "building a tree" in {

      sealed trait Tree
      final case class Leaf(label: String) extends Tree
      final case class Branch(label: String, left: Tree, right: Tree) extends Tree

      val tree = cy2a.fold[Tree](
        hNil = () => Leaf("HNil"),
        hCons = (_, _, _, h, t) => Branch("HCons", h, t),
        cNil = () => Leaf("CNil"),
        cCons = (_, _, h, t) => Branch("CCons", h, t),
        vector = (_, e) => Branch("Vector", e, Leaf("⊥")),
        atom = (a) => Leaf(s"$a"),
        cycle = () => Leaf("CYCLE")
      )

      val expected = Branch("HCons",
        Branch("CCons",
          Branch("HCons", Leaf(atom[String].toString), Leaf("HNil")),
          Branch("CCons",
            Branch("HCons",
              Leaf("CYCLE"),
              Branch("HCons", Leaf(atom[String].toString), Leaf("HNil"))
            ),
            Leaf("CNil")
          )
        ),
        Branch("HCons", Leaf(atom[MyUUID].toString), Leaf("HNil"))
      )

      tree should === (expected)
    }

    "computing with a monad" in {

      import cats.data.State

      def murMix(a: Int, b: Int): Int = {
        MurmurHash3.mix(a, b)
      }

      def mix(i: Int): State[Int, Unit] = for {
        curr <- State.get[Int]
        mixed = murMix(curr, i)
        _ <- State.set(mixed)
      } yield ()

      val computation = cy2a.fold[State[Int, Unit]](
        hNil = () => mix(1),
        hCons = (_, _, _, h, t) => for {
          _ <- h
          _ <- t
          _ <- mix(2)
        } yield (),
        cNil = () => mix(3),
        cCons = (_, _, h, t) => for {
          _ <- h
          _ <- t
          _ <- mix(4)
        } yield (),
        vector = (_, e) => e,
        atom = (a) => mix(a.uuid.##),
        cycle = () => State.pure(())
      )

      val a = atom[String].uuid.##
      val b = atom[MyUUID].uuid.##
      val expected = for {
        _ <- mix(a)
        _ <- mix(1)
        _ <- mix(2)
        _ <- mix(a)
        _ <- mix(1)
        _ <- mix(2)
        _ <- mix(2)
        _ <- mix(3)
        _ <- mix(4)
        _ <- mix(4)
        _ <- mix(b)
        _ <- mix(1)
        _ <- mix(2)
        _ <- mix(2)
      } yield ()

      val hash = computation.runS(0).value
      val exp = expected.runS(0).value
      hash should === (exp)
    }
  }
}

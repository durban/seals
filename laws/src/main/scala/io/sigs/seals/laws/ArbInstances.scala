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
package laws

import java.util.UUID

import shapeless._

import cats.implicits._

import scodec.bits.{ ByteVector, BitVector }

import org.scalacheck.{ Arbitrary, Gen, Cogen }
import org.scalacheck.derive.Recursive

import core.Refinement.{ Semantics, ReprFormat }

object ArbInstances extends ArbInstances

trait ArbInstances {

  implicit def arbUuid(implicit al: Arbitrary[Long]): Arbitrary[UUID] =
    Arbitrary(Gen.uuid)

  implicit val cogenUuid: Cogen[UUID] =
    Cogen[String].contramap(_.toString)

  implicit val arbMathContext: Arbitrary[java.math.MathContext] = {
    Arbitrary {
      for {
        precision <- Gen.chooseNum(0, Int.MaxValue)
        rounding <- Gen.oneOf(java.math.RoundingMode.values())
      } yield new java.math.MathContext(precision, rounding)
    }
  }

  implicit def cogenMathContext(implicit cgStr: Cogen[String]): Cogen[java.math.MathContext] =
    cgStr.contramap(_.toString)

  implicit def arbSymbol(implicit arbStr: Arbitrary[String]): Arbitrary[Symbol] = Arbitrary {
    arbStr.arbitrary.map(Symbol.apply)
  }

  implicit def cogenSymbol(implicit cgStr: Cogen[String]): Cogen[Symbol] =
    cgStr.contramap(_.name)

  implicit def arbByteVector(implicit arbArr: Arbitrary[Array[Byte]]): Arbitrary[ByteVector] = Arbitrary {
    arbArr.arbitrary.map(ByteVector.view)
  }

  implicit def arbBitVector(implicit arbBytes: Arbitrary[ByteVector], arbByte: Arbitrary[Byte]): Arbitrary[BitVector] = Arbitrary {
    Gen.oneOf(
      arbBytes.arbitrary.map(_.bits),
      for {
        bytes <- arbBytes.arbitrary
        byte <- arbByte.arbitrary
        slice <- Gen.choose(1L, 7L)
      } yield bytes.bits ++ BitVector.fromByte(byte, 8).take(slice)
    )
  }

  implicit def arbEnvelope[A: Reified](implicit A: Arbitrary[A]): Arbitrary[Envelope[A]] = {
    Arbitrary {
      for {
        v <- A.arbitrary
      } yield Envelope(v)
    }
  }

  implicit def cogenEnvelope[A](implicit A: Cogen[A]): Cogen[Envelope[A]] = {
    Cogen.apply { (seed, env) =>
      A.perturb(seed, env.value).reseed(Envelope.hashSeed)
    }
  }

  implicit def arbReified[A](implicit r: Reified[A]): Arbitrary[Reified[A]] = Arbitrary {
    Gen.const(r)
  }

  implicit def cogenReified[A]: Cogen[Reified[A]] =
    cogenModel.contramap(_.model)

  implicit def arbAtomic[A](implicit a: Atomic[A]): Arbitrary[Atomic[A]] = Arbitrary {
    Gen.const(a)
  }

  implicit def cogenAtomic[A]: Cogen[Atomic[A]] =
    Cogen[Int].contramap(_.##)

  implicit def arbKleene[F[_]](implicit k: Kleene[F]): Arbitrary[Kleene[F]] = Arbitrary {
    Gen.const(k)
  }

  implicit def arbEnumLike[A](implicit enu: core.EnumLike[A]): Arbitrary[core.EnumLike[A]] = Arbitrary {
    Gen.const(enu)
  }

  implicit def cogenEnumLike[A](implicit enu: core.EnumLike[A]): Cogen[A] =
    Cogen[Int].contramap(a => enu.index(a))

  implicit def arbSemantics(implicit uid: Arbitrary[UUID]): Arbitrary[Semantics] = Arbitrary {
    uid.arbitrary.map(id => Semantics(id, ReprFormat("(", true, sh"){${id}}")))
  }

  implicit def arbRefineForModel(implicit arbSem: Arbitrary[Semantics]): Arbitrary[Option[Semantics]] = Arbitrary {
    for {
      rnd <- Gen.choose(0, 3)
      refine = if (rnd === 0) true else false
      refinement <- if (refine) {
        arbSem.arbitrary.map(Some(_))
      } else {
        Gen.const(None)
      }
    } yield refinement
  }

  implicit def arbModelHlist(implicit arbM: Lazy[Arbitrary[Model]]): Arbitrary[Model.HList] = Arbitrary {
    Gen.oneOf(arbModelHnil.arbitrary, Gen.lzy(arbModelHcons.arbitrary))
  }

  implicit def arbModelHnil: Arbitrary[Model.HNil.type] = Arbitrary {
    Arbitrary.arbUnit.arbitrary.map(_ => Model.HNil)
  }

  implicit def arbModelHcons(implicit arbM: Lazy[Arbitrary[Model]], arbRef: Arbitrary[Option[Semantics]]): Arbitrary[Model.HCons[_]] = Arbitrary {
    for {
      sym <- Gen.alphaStr
      opt <- Gen.oneOf(true, false)
      h <- Gen.lzy(arbM.value.arbitrary)
      t <- Gen.lzy(arbModelHlist(arbM).arbitrary)
      refinement <- arbRef.arbitrary
    } yield Model.HCons(Symbol(sym), opt, refinement, h, t)
  }

  implicit def arbModelCoproduct(implicit arbM: Lazy[Arbitrary[Model]]): Arbitrary[Model.Coproduct] = Arbitrary {
    Gen.oneOf(arbModelCnil.arbitrary, Gen.lzy(arbModelCcons.arbitrary))
  }

  implicit def arbModelCnil: Arbitrary[Model.CNil.type] = Arbitrary {
    Arbitrary.arbUnit.arbitrary.map(_ => Model.CNil)
  }

  implicit def arbModelCcons(implicit arbM: Lazy[Arbitrary[Model]], arbRef: Arbitrary[Option[Semantics]]): Arbitrary[Model.CCons] = Arbitrary {
    for {
      sym <- Gen.alphaStr
      h <- Gen.lzy(arbM.value.arbitrary)
      t <- Gen.lzy(arbModelCoproduct(arbM).arbitrary)
      refinement <- arbRef.arbitrary
    } yield Model.CCons(Symbol(sym), refinement, h, t)
  }

  implicit def arbModelVector(implicit arbM: Lazy[Arbitrary[Model]], arbRef: Arbitrary[Option[Semantics]]): Arbitrary[Model.Vector] = Arbitrary {
    for {
      e <- Gen.lzy(arbM.value.arbitrary)
      refinement <- arbRef.arbitrary
    } yield Model.Vector(e, refinement)
  }

  def unrefinedArbModelAtom: Arbitrary[Model.Atom] = Arbitrary {
    Gen.oneOf(core.Atomic.registry.values.toSeq)
  }

  implicit def arbModelAtom(implicit arbRef: Arbitrary[Option[Semantics]]): Arbitrary[Model.Atom] = Arbitrary {
    for {
      a <- this.unrefinedArbModelAtom.arbitrary
      ref <- arbRef.arbitrary
    } yield ref.fold(a)(r => Model.CanBeRefined.atomCanBeRefined.refine(a, r))
  }

  implicit def recModel: Recursive[Model] =
    Recursive(arbModelAtom.arbitrary)

  implicit def arbModel: Arbitrary[Model] = {
    Arbitrary {
      Gen.sized { oldSize =>
        val newSize = Math.sqrt(oldSize.toDouble).ceil.toInt
        Gen.resize(newSize, Gen.lzy(_arbModel.arbitrary))
      }
    }
  }

  private[this] lazy val _arbModel: Arbitrary[Model] = {

    type ModelReprH = Model.HNil.type
    type ModelReprT = Model.HCons[_] :+: Model.CNil.type :+: Model.CCons :+: Model.Atom :+: Model.Vector :+: CNil
    type ModelRepr = ModelReprH :+: ModelReprT

    implicit val modGen: Generic.Aux[Model, ModelRepr] = new Generic[Model] {
      type Repr = ModelRepr
      def from(r: Repr): Model =
        shapeless.ops.coproduct.Unifier[Repr].apply(r)
      def to(t: Model): Repr = t match {
        case Model.HNil => Inl(Model.HNil)
        case hc: core.Model.HCons[_] => Inr(Inl(hc))
        case Model.CNil => Inr(Inr(Inl(Model.CNil)))
        case cc: core.Model.CCons => Inr(Inr(Inr(Inl(cc))))
        case a: core.Model.Atom => Inr(Inr(Inr(Inr(Inl(a)))))
        case v: core.Model.Vector => Inr(Inr(Inr(Inr(Inr(Inl(v))))))
      }
    }

    locally {
      import org.scalacheck.ScalacheckShapeless._
      val arbModel = derivedArbitrary[Model](
        null : LowPriority, // scalastyle:ignore null
        org.scalacheck.derive.MkArbitrary[Model]
      )
      arbModel
    }
  }

  implicit def cogenModel: Cogen[Model] =
    Cogen[String].contramap(_.toString)
}

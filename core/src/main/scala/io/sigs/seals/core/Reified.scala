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
package core

import cats.{ Monad, InvariantMonoidal }
import cats.data.Xor
import shapeless._
import shapeless.labelled.{ field, FieldType }
import shapeless.ops.hlist.ToTraversable

sealed trait Reified[A] extends Serializable { self =>

  type Mod <: Model

  type Fold[B, T]

  final def model: Model =
    modelComponent

  private[core] def modelComponent: Mod

  final override def toString: String =
    s"Reified[${model.desc}]"

  final override def equals(that: Any): Boolean = that match {
    case that: AnyRef =>
      this eq that
    case _ =>
      false
  }

  final override def hashCode: Int =
    System.identityHashCode(this)

  def genFold[B, T](a: A)(
    atom: String => B,
    hNil: () => T,
    hCons: (Symbol, B, T) => T,
    prod: T => B,
    sum: (Symbol, B) => B,
    vector: Vector[B] => B
  ): Fold[B, T]

  def fold[B](a: A)(
    atom: String => B,
    hNil: () => B,
    hCons: (Symbol, B, B) => B,
    sum: (Symbol, B) => B,
    vector: Vector[B] => B
  ): B = close[B, B](genFold[B, B](a)(atom, hNil, hCons, identity, sum, vector), identity)

  def close[B, T](x: Fold[B, T], f: T => B): B

  // TODO: try to remove one layer of Xor (hCons)
  def unfold[B, E, S](
    atom: B => Xor[E, (String, B)],
    atomErr: B => E,
    hNil: B => Xor[E, B],
    hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
    cNil: B => E,
    cCons: (B, Symbol) => Xor[E, Either[B, B]],
    vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
  )(b: B): Xor[E, (A, B)]

  def imap[B](f: A => B)(g: B => A): Reified.Aux[B, self.Mod, self.Fold] = new Reified[B] {

    override type Mod = self.Mod
    override type Fold[C, T] = self.Fold[C, T]

    override private[core] def modelComponent: Mod =
      self.modelComponent

    override def genFold[C, T](b: B)(
      atom: String => C,
      hNil: () => T,
      hCons: (Symbol, C, T) => T,
      prod: T => C,
      sum: (Symbol, C) => C,
      vector: Vector[C] => C
    ): Fold[C, T] = self.genFold[C, T](g(b))(atom, hNil, hCons, prod, sum, vector)

    override def close[C, T](x: Fold[C, T], f: T => C): C =
      self.close(x, f)

    override def unfold[C, E, S](
      atom: C => Xor[E, (String, C)],
      atomErr: C => E,
      hNil: C => Xor[E, C],
      hCons: (C, Symbol) => Xor[E, Xor[E, (C, C => Xor[E, C])]],
      cNil: C => E,
      cCons: (C, Symbol) => Xor[E, Either[C, C]],
      vector: C => Xor[E, (C, S, (C, S) => Xor[E, Option[(C, S)]])]
    )(c: C): Xor[E, (B, C)] = self.unfold[C, E, S](atom, atomErr, hNil, hCons, cNil, cCons, vector)(c).map {
      case (a, c) => (f(a), c)
    }
  }

  private[core] def unsafeWithDefaults(defs: List[Option[Any]]): Reified.Aux[A, this.Mod, this.Fold] =
    this
}

object Reified extends LowPrioReified {

  final val hashSeed = 0xf2fe2bb5

  type Aux[A, M <: Model, F[_, _]] = Reified[A] {
    type Mod = M
    type Fold[B, T] = F[B, T]
  }

  type Aux2[A, M <: Model] = Reified[A] {
    type Mod = M
  }

  type FFirst[B, T] = B
  type FSecond[B, T] = T

  def apply[A](implicit d: Reified[A]): Aux[A, d.Mod, d.Fold] =
    d

  implicit val reifiedInvariantMonoidalFunctor: InvariantMonoidal[Reified] = new InvariantMonoidal[Reified] {

    type Fst = Witness.`'_1`.T
    type Snd = Witness.`'_2`.T

    def pure[A](a: A): Reified[A] =
      Reified[HNil].imap(_ => a)(_ => HNil)

    def imap[A, B](fa: Reified[A])(f: A => B)(g: B => A): Reified[B] =
      fa.imap(f)(g)

    def product[A, B](fa: Reified[A], fb: Reified[B]): Reified[(A, B)] = {
      implicit val ra: Reified[A] = fa
      implicit val rb: Reified[B] = fb
      Reified[FieldType[Fst, A] :: FieldType[Snd, B] :: HNil].imap[(A, B)] { hl =>
        (hl.head, hl.tail.head)
      } { case (a, b) =>
        field[Fst](a) :: field[Snd](b) :: HNil
      }
    }
  }

  implicit def reifiedFromAtom[A](
    implicit A: Atom[A]
  ): Reified.Aux[A, Atom[A], FFirst] = new Reified[A] {
    override type Mod = Atom[A]
    override type Fold[B, T] = B
    private[core] override val modelComponent = A
    override def genFold[B, T](a: A)(
      atom: String => B,
      hNil: () => T,
      hCons: (Symbol, B, T) => T,
      prod: T => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): B = atom(this.modelComponent.stringRepr(a))
    override def close[B, T](x: Fold[B, T], f: T => B): B =
      x
    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (A, B)] = {
      atom(b).flatMap { case (str, b) =>
        Xor.fromOption(
          A.fromString(str).map(a => (a, b)),
          atomErr(b)
        )
      }
    }
  }

  implicit def reifiedFromKleene[F[_], A](
    implicit
    F: Kleene[F],
    R: Reified[A]
  ): Reified.Aux[F[A], Model.Vector, FFirst] = new Reified[F[A]] {
    override type Mod = Model.Vector
    override type Fold[B, T] = B
    private[core] override val modelComponent = Model.Vector(R.modelComponent)
    override def genFold[B, T](a: F[A])(
      atom: String => B,
      hNil: () => T,
      hCons: (Symbol, B, T) => T,
      prod: T => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): Fold[B, T] = {
      vector(F.toVector(a).map(x => R.close(R.genFold[B, T](x)(atom, hNil, hCons, prod, sum, vector), prod)))
    }
    override def close[B, T](x: Fold[B, T], f: T => B): B =
      x
    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (F[A], B)] = {
      vector(b).flatMap { case (b, s, f) =>
        val rec = Monad[Xor[E, ?]].tailRecM((b, s, Vector.empty[A])) { case (b, s, v) =>
          f(b, s).flatMap {
            case Some((b, s)) =>
              val b2 = R.unfold[B, E, S](atom, atomErr, hNil, hCons, cNil, cCons, vector)(b)
              b2.map { case (a, b) =>
                Left((b, s, v :+ a))
              }
            case None =>
              Xor.right(Right((b, s, v)))
          }
        }

        rec.map { case (b, _, v) =>
          (F.fromVector(v), b)
        }
      }
    }
  }
}

private[core] sealed trait LowPrioReified {

  import Reified.{ FFirst, FSecond }

  implicit def reifiedFromHcons[HK <: Symbol, HV, T <: HList, MT <: Model.HList](
    implicit
    label: Witness.Aux[HK],
    rh0: Lazy[Reified[HV]],
    rt0: Lazy[Reified.Aux[T, MT, FSecond]]
  ): Reified.Aux[FieldType[HK, HV] :: T, Model.HCons, FSecond] = {
    new HConsReified[HK, HV, T, MT](label, None, () => rh0, () => rt0)
  }

  private final class HConsReified[HK <: Symbol, HV, T <: HList, MT <: Model.HList](
    private[this] val label: Witness.Aux[HK],
    private[this] val headDefault: Option[HV],
    rh0: () => Lazy[Reified[HV]],
    rt0: () => Lazy[Reified.Aux[T, MT, FSecond]]
  )   extends WithLazy[Reified[HV], Reified.Aux[T, MT, FSecond]](rh0, rt0)
      with Reified[FieldType[HK, HV] :: T] {

    override type Mod = Model.HCons
    override type Fold[X, Y] = Y

    lazy private[core] override val modelComponent: Mod =
      Model.HCons(label.value, head.modelComponent, tail.modelComponent)

    private[core] override def unsafeWithDefaults(defs: List[Option[Any]]): Reified.Aux[FieldType[HK, HV] :: T, Model.HCons, FSecond] = {
      new HConsReified[HK, HV, T, MT](
        label,
        defs.head.map(_.asInstanceOf[HV]),
        () => head,
        () => tail.unsafeWithDefaults(defs.tail)
      )
    }

    override def genFold[B, U](a: FieldType[HK, HV] :: T)(
      atom: String => B,
      hNil: () => U,
      hCons: (Symbol, B, U) => U,
      prod: U => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): Fold[B, U] = {
      val h = head.close(head.genFold(a.head)(atom, hNil, hCons, prod, sum, vector), prod)
      val t = tail.genFold(a.tail)(atom, hNil, hCons, prod, sum, vector)
      hCons(label.value, h, t)
    }

    override def close[B, U](x: Fold[B, U], f: U => B): B =
      f(x)

    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (FieldType[HK, HV] :: T, B)] = {
      for {
        ht <- hCons(b, label.value).fold[Xor[E, (HV, B)]](
          missingField => {
            Xor.fromOption[E, HV](headDefault, missingField).map(h => (h, b))
          },
          ok => ok.flatMap { case (hb, f) =>
            head.unfold(atom, atomErr, hNil, hCons, cNil, cCons, vector)(hb).flatMap {
              case (h, b) => f(b).map(b => (h, b))
            }
          }
        )
        (h, tb) = ht
        tb <- tail.unfold(atom, atomErr, hNil, hCons, cNil, cCons, vector)(tb)
      } yield {
        val (t, b) = tb
        (field[HK](h) :: t, b)
      }
    }
  }

  implicit val reifiedFromHnil: Reified.Aux[HNil, Model.HNil.type, FSecond] = {
    new Reified[HNil] {
      override type Mod = Model.HNil.type
      override type Fold[X, Y] = Y
      private[core] override val modelComponent: Mod = Model.HNil
      override def genFold[B, U](a: HNil)(
        atom: String => B,
        hNil: () => U,
        hCons: (Symbol, B, U) => U,
        prod: U => B,
        sum: (Symbol, B) => B,
        vector: Vector[B] => B
      ): Fold[B, U] = hNil()
      override def close[B, U](x: Fold[B, U], f: U => B): B =
        f(x)
      override def unfold[B, E, S](
        atom: B => Xor[E, (String, B)],
        atomErr: B => E,
        hNil: B => Xor[E, B],
        hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
        cNil: B => E,
        cCons: (B, Symbol) => Xor[E, Either[B, B]],
        vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
      )(b: B): Xor[E, (HNil, B)] = {
        hNil(b).map(b => (HNil, b))
      }
    }
  }

  implicit def reifiedFromCcons[HK <: Symbol, HV, T <: Coproduct, MT <: Model.Coproduct](
    implicit
    label: Witness.Aux[HK],
    rh0: Lazy[Reified[HV]],
    rt0: Lazy[Reified.Aux2[T, MT]]
  ): Reified.Aux[FieldType[HK, HV] :+: T, Model.CCons, FFirst] = new WithLazy[Reified[HV], Reified.Aux2[T, MT]](
      () => rh0,
      () => rt0
  ) with Reified[FieldType[HK, HV] :+: T] {
    override type Mod = Model.CCons
    override type Fold[B, X] = B
    lazy private[core] override val modelComponent: Mod = Model.CCons(label.value, head.modelComponent, tail.modelComponent)
    override def genFold[B, U](a: FieldType[HK, HV] :+: T)(
      atom: String => B,
      hNil: () => U,
      hCons: (Symbol, B, U) => U,
      prod: U => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): Fold[B, U] = a match {
      case Inl(left) =>
        val l = head.genFold[B, U](left)(atom, hNil, hCons, prod, sum, vector)
        sum(label.value, head.close(l, prod))
      case Inr(right) =>
        tail.close(tail.genFold[B, U](right)(atom, hNil, hCons, prod, sum, vector), prod)
    }
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      x
    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (FieldType[HK, HV] :+: T, B)] = {
      for {
        e <- cCons(b, label.value)
        x <- e match {
          case Left(h) =>
            head.unfold(atom, atomErr, hNil, hCons, cNil, cCons, vector)(h).map {
              case (h, b) => (Inl[FieldType[HK, HV], T](field[HK](h)), b)
            }
          case Right(t) =>
            tail.unfold(atom, atomErr, hNil, hCons, cNil, cCons, vector)(t).map {
              case (t, b) => (Inr[FieldType[HK, HV], T](t), b)
            }
        }
      } yield x
    }
  }

  implicit val reifiedFromCnil: Reified.Aux[CNil, Model.CNil.type, FFirst] = new Reified[CNil] {
    override type Mod = Model.CNil.type
    override type Fold[B, U] = B
    private[core] override val modelComponent: Mod = Model.CNil
    override def genFold[B, U](a: CNil)(
      atom: String => B,
      hNil: () => U,
      hCons: (Symbol, B, U) => U,
      prod: U => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): Fold[B, U] = a.impossible
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      x
    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (CNil, B)] = Xor.left(cNil(b))
  }

  // TODO: maybe do this with imap (?)
  implicit def reifiedFromGenericProductWithDefaults[A, GA <: HList, DA <: HList, MA <: Model.HList, FA[_, _]](
    implicit
    A: LabelledGeneric.Aux[A, GA],
    D: Default.Aux[A, DA],
    DL: ToTraversable.Aux[DA, List, Option[Any]],
    WD: Model.WithDefaults[DA],
    RA: Lazy[Reified.Aux[GA, MA, FA]]
  ): Reified.Aux[A, Model.HList, FA] = new WithLazy[Reified[GA] { type Mod = MA; type Fold[X, Y] = FA[X, Y] }, None.type](
      () => RA.map(_.unsafeWithDefaults(D.apply().toList)),
      () => None
  ) with Reified[A] {
    override type Mod = Model.HList
    override type Fold[B, U] = FA[B, U]
    lazy private[core] override val modelComponent: Mod = {
      WD.unsafeWithDefaults(head.modelComponent)(D())
    }
    override def genFold[B, U](a: A)(
      atom: String => B,
      hNil: () => U,
      hCons: (Symbol, B, U) => U,
      prod: U => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): Fold[B, U] = {
      head.genFold(A.to(a))(atom, hNil, hCons, prod, sum, vector)
    }
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      RA.value.close(x, f)
    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (A, B)] = {
      head.unfold(atom, atomErr, hNil, hCons, cNil, cCons, vector)(b).map {
        case (ga, b) => (A.from(ga), b)
      }
    }
  }

  // TODO: maybe do this with imap (?)
  implicit def reifiedFromGenericCoproduct[A, GA <: Coproduct, MA <: Model, FA[_, _]](
    implicit
    A: LabelledGeneric.Aux[A, GA],
    RA: Lazy[Reified.Aux[GA, MA, FA]]
  ): Reified.Aux[A, MA, FA] = new WithLazy[Reified[GA] { type Mod = MA; type Fold[X, Y] = FA[X, Y] }, None.type](
      () => RA,
      () => None
  ) with Reified[A] {
    override type Mod = MA
    override type Fold[B, U] = FA[B, U]
    lazy private[core] override val modelComponent: Mod = head.modelComponent
    override def genFold[B, U](a: A)(
      atom: String => B,
      hNil: () => U,
      hCons: (Symbol, B, U) => U,
      prod: U => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B
    ): Fold[B, U] = {
      head.genFold(A.to(a))(atom, hNil, hCons, prod, sum, vector)
    }
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      RA.value.close(x, f)
    override def unfold[B, E, S](
      atom: B => Xor[E, (String, B)],
      atomErr: B => E,
      hNil: B => Xor[E, B],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B => Xor[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]],
      vector: B => Xor[E, (B, S, (B, S) => Xor[E, Option[(B, S)]])]
    )(b: B): Xor[E, (A, B)] = {
      head.unfold(atom, atomErr, hNil, hCons, cNil, cCons, vector)(b).map {
        case (ga, b) => (A.from(ga), b)
      }
    }
  }
}

private sealed abstract class WithLazy[H, T](
    private[this] var lazyHead: () => Lazy[H],
    private[this] var lazyTail: () => Lazy[T]
) extends Serializable { this: Reified[_] =>

  protected lazy val head = {
    val res = lazyHead().value
    lazyHead = null // scalastyle:ignore
    res
  }

  protected lazy val tail = {
    val res = lazyTail().value
    lazyTail = null // scalastyle:ignore
    res
  }

  @throws[java.io.IOException]
  private def writeObject(out: java.io.ObjectOutputStream): Unit = {
    // force evaluation of the lazy val thunks,
    // to clean any captured state which we
    // don't want to serialize:
    this.head
    this.tail
    // now it should work by default:
    out.defaultWriteObject()
  }
}

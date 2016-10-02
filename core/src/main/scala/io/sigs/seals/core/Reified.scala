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

import cats.InvariantMonoidal
import cats.data.Xor
import shapeless._
import shapeless.labelled.{ field, FieldType }
import shapeless.ops.hlist.ToTraversable

sealed trait Reified[A] extends Serializable { self =>

  type Mod <: Model

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

  def fold[B](a: A)(
    atom: String => B,
    hNil: () => B,
    hCons: (Symbol, B, B) => B,
    sum: (Symbol, B) => B
  ): B

  def unfold[B, E](
    atom: B => Xor[E, String],
    atomErr: B => E,
    hNil: B => Xor[E, Unit],
    hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
    cNil: B => E,
    cCons: (B, Symbol) => Xor[E, Either[B, B]]
  )(b: B): Xor[E, A]

  def imap[B](f: A => B)(g: B => A): Reified.Aux[B, self.Mod] = new Reified[B] {

    override type Mod = self.Mod

    override private[core] def modelComponent: Mod =
      self.modelComponent

    def fold[C](b: B)(
      atom: String => C,
      hNil: () => C,
      hCons: (Symbol, C, C) => C,
      sum: (Symbol, C) => C
    ): C = self.fold[C](g(b))(atom, hNil, hCons, sum)

    def unfold[C, E](
      atom: C => Xor[E, String],
      atomErr: C => E,
      hNil: C => Xor[E, Unit],
      hCons: (C, Symbol) => Xor[E, Xor[E, (C, C)]],
      cNil: C => E,
      cCons: (C, Symbol) => Xor[E, Either[C, C]]
    )(c: C): Xor[E, B] = self.unfold[C, E](atom, atomErr, hNil, hCons, cNil, cCons)(c).map(f)
  }

  private[core] def unsafeWithDefaults(defs: List[Option[Any]]): Reified.Aux[A, this.Mod] =
    this
}

object Reified extends LowPrioReified {

  final val hashSeed = 0xf2fe2bb5

  type Aux[A, M <: Model] = Reified[A] {
    type Mod = M
  }

  def apply[A](implicit d: Reified[A]): Aux[A, d.Mod] =
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
  ): Reified.Aux[A, Atom[A]] = new Reified[A] {
    override type Mod = Atom[A]
    private[core] override val modelComponent = A
    override def fold[B](a: A)(
      atom: String => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B
    ): B = atom(this.modelComponent.stringRepr(a))
    override def unfold[B, E](
      atom: B => Xor[E, String],
      atomErr: B => E,
      hNil: B => Xor[E, Unit],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]]
    )(b: B): Xor[E, A] = {
      atom(b).flatMap { str =>
        Xor.fromOption(
          A.fromString(str),
          atomErr(b)
        )
      }
    }
  }
}

private[core] sealed trait LowPrioReified {

  implicit def reifiedFromHcons[HK <: Symbol, HV, T <: HList, MT <: Model.HList](
    implicit
    label: Witness.Aux[HK],
    rh0: Lazy[Reified[HV]],
    rt0: Lazy[Reified.Aux[T, MT]]
  ): Reified.Aux[FieldType[HK, HV] :: T, Model.HCons] = {
    new HConsReified[HK, HV, T, MT](label, None, () => rh0, () => rt0)
  }

  private final class HConsReified[HK <: Symbol, HV, T <: HList, MT <: Model.HList](
    private[this] val label: Witness.Aux[HK],
    private[this] val headDefault: Option[HV],
    rh0: () => Lazy[Reified[HV]],
    rt0: () => Lazy[Reified.Aux[T, MT]]
  )   extends WithLazy[Reified[HV], Reified.Aux[T, MT]](rh0, rt0)
      with Reified[FieldType[HK, HV] :: T] {

    override type Mod = Model.HCons

    lazy private[core] override val modelComponent: Mod =
      Model.HCons(label.value, head.modelComponent, tail.modelComponent)

    private[core] override def unsafeWithDefaults(defs: List[Option[Any]]): Reified.Aux[FieldType[HK, HV] :: T, Model.HCons] = {
      new HConsReified[HK, HV, T, MT](
        label,
        defs.head.map(_.asInstanceOf[HV]),
        () => head,
        () => tail.unsafeWithDefaults(defs.tail)
      )
    }

    override def fold[B](a: FieldType[HK, HV] :: T)(
      atom: String => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B
    ): B = {
      val h = head.fold(a.head)(atom, hNil, hCons, sum)
      val t = tail.fold(a.tail)(atom, hNil, hCons, sum)
      hCons(label.value, h, t)
    }

    override def unfold[B, E](
      atom: B => Xor[E, String],
      atomErr: B => E,
      hNil: B => Xor[E, Unit],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]]
    )(b: B): Xor[E, FieldType[HK, HV] :: T] = {
      for {
        ht <- hCons(b, label.value).fold[Xor[E, (HV, B)]](
          missingField => {
            Xor.fromOption[E, HV](headDefault, missingField).map(h => (h, b))
          },
          ok => ok.flatMap { case (hb, tb) =>
            head.unfold(atom, atomErr, hNil, hCons, cNil, cCons)(hb).map(h => (h, tb))
          }
        )
        (h, tb) = ht
        t <- tail.unfold(atom, atomErr, hNil, hCons, cNil, cCons)(tb)
      } yield field[HK](h) :: t
    }
  }

  implicit val reifiedFromHnil: Reified.Aux[HNil, Model.HNil.type] = {
    new Reified[HNil] {
      override type Mod = Model.HNil.type
      private[core] override val modelComponent: Mod = Model.HNil
      override def fold[B](a: HNil)(
        atom: String => B,
        hNil: () => B,
        hCons: (Symbol, B, B) => B,
        sum: (Symbol, B) => B
      ): B = hNil()
      override def unfold[B, E](
        atom: B => Xor[E, String],
        atomErr: B => E,
        hNil: B => Xor[E, Unit],
        hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
        cNil: B => E,
        cCons: (B, Symbol) => Xor[E, Either[B, B]]
      )(b: B): Xor[E, HNil] = {
        hNil(b).map(_ => HNil)
      }
    }
  }

  implicit def reifiedFromCcons[HK <: Symbol, HV, T <: Coproduct, MT <: Model.Coproduct](
    implicit
    label: Witness.Aux[HK],
    rh0: Lazy[Reified[HV]],
    rt0: Lazy[Reified.Aux[T, MT]]
  ): Reified.Aux[FieldType[HK, HV] :+: T, Model.CCons] = new WithLazy[Reified[HV], Reified.Aux[T, MT]](
      () => rh0,
      () => rt0
  ) with Reified[FieldType[HK, HV] :+: T] {
    override type Mod = Model.CCons
    lazy private[core] override val modelComponent: Mod = Model.CCons(label.value, head.modelComponent, tail.modelComponent)
    override def fold[B](a: FieldType[HK, HV] :+: T)(
      atom: String => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B
    ): B = a match {
      case Inl(left) =>
        val l = head.fold(left)(atom, hNil, hCons, sum)
        sum(label.value, l)
      case Inr(right) =>
        tail.fold(right)(atom, hNil, hCons, sum)
    }
    override def unfold[B, E](
      atom: B => Xor[E, String],
      atomErr: B => E,
      hNil: B => Xor[E, Unit],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]]
    )(b: B): Xor[E, FieldType[HK, HV] :+: T] = {
      for {
        e <- cCons(b, label.value)
        x <- e match {
          case Left(h) =>
            head.unfold(atom, atomErr, hNil, hCons, cNil, cCons)(h).map { h =>
              Inl[FieldType[HK, HV], T](field[HK](h))
            }
          case Right(t) =>
            tail.unfold(atom, atomErr, hNil, hCons, cNil, cCons)(t).map { t =>
              Inr[FieldType[HK, HV], T](t)
            }
        }
      } yield x
    }
  }

  implicit val reifiedFromCnil: Reified.Aux[CNil, Model.CNil.type] = new Reified[CNil] {
    override type Mod = Model.CNil.type
    private[core] override val modelComponent: Mod = Model.CNil
    override def fold[B](a: CNil)(
      atom: String => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B
    ): B = a.impossible
    override def unfold[B, E](
      atom: B => Xor[E, String],
      atomErr: B => E,
      hNil: B => Xor[E, Unit],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]]
    )(b: B): Xor[E, CNil] = Xor.left(cNil(b))
  }

  implicit def reifiedFromGenericProductWithDefaults[A, GA <: HList, DA <: HList, MA <: Model.HList](
    implicit
    A: LabelledGeneric.Aux[A, GA],
    D: Default.Aux[A, DA],
    DL: ToTraversable.Aux[DA, List, Option[Any]],
    WD: Model.WithDefaults[DA],
    RA: Lazy[Reified.Aux[GA, MA]]
  ): Reified.Aux[A, Model.HList] = new WithLazy[Reified[GA] { type Mod = MA }, None.type](
      () => RA.map(_.unsafeWithDefaults(D.apply().toList)),
      () => None
  ) with Reified[A] {
    override type Mod = Model.HList
    lazy private[core] override val modelComponent: Mod = {
      WD.unsafeWithDefaults(head.modelComponent)(D())
    }
    override def fold[B](a: A)(
      atom: String => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B
    ): B = {
      head.fold(A.to(a))(atom, hNil, hCons, sum)
    }
    override def unfold[B, E](
      atom: B => Xor[E, String],
      atomErr: B => E,
      hNil: B => Xor[E, Unit],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]]
    )(b: B): Xor[E, A] = {
      head.unfold(atom, atomErr, hNil, hCons, cNil, cCons)(b).map(A.from)
    }
  }

  implicit def reifiedFromGenericCoproduct[A, GA <: Coproduct, MA <: Model](
    implicit
    A: LabelledGeneric.Aux[A, GA],
    RA: Lazy[Reified.Aux[GA, MA]]
  ): Reified.Aux[A, MA] = new WithLazy[Reified[GA] { type Mod = MA }, None.type](
      () => RA,
      () => None
  ) with Reified[A] {
    override type Mod = MA
    lazy private[core] override val modelComponent: Mod = head.modelComponent
    override def fold[B](a: A)(
      atom: String => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B
    ): B = {
      head.fold(A.to(a))(atom, hNil, hCons, sum)
    }
    override def unfold[B, E](
      atom: B => Xor[E, String],
      atomErr: B => E,
      hNil: B => Xor[E, Unit],
      hCons: (B, Symbol) => Xor[E, Xor[E, (B, B)]],
      cNil: B => E,
      cCons: (B, Symbol) => Xor[E, Either[B, B]]
    )(b: B): Xor[E, A] = {
      head.unfold(atom, atomErr, hNil, hCons, cNil, cCons)(b).map(A.from)
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

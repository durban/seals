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

import java.util.UUID
import java.math.{ MathContext, RoundingMode }

import cats.{ Monad, InvariantMonoidal, Order }
import cats.implicits._

import shapeless._
import shapeless.union._
import shapeless.record._
import shapeless.labelled.{ field, FieldType }
import shapeless.ops.hlist.ToTraversable
import shapeless.ops.nat.ToInt

import scodec.bits.ByteVector

import Reified.{ Folder, Unfolder }

sealed trait Reified[A] extends Serializable { self =>

  type Mod <: Model

  type Fold[B, T]

  final def model: Model =
    modelComponent

  private[core] def modelComponent: Mod

  final override def toString: String =
    sh"Reified[${model.desc}]"

  final override def equals(that: Any): Boolean = that match {
    case that: AnyRef =>
      this eq that
    case _ =>
      false
  }

  final override def hashCode: Int =
    System.identityHashCode(this)

  def fold[B, T](a: A)(f: Folder[B, T]): Fold[B, T]

  def close[B, T](x: Fold[B, T], f: T => B): B

  final def foldClose[B](a: A)(f: Folder[B, B]): B =
    close[B, B](fold(a)(f), identity)

  def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (A, B)]

  final def imap[B](f: A => B)(g: B => A): Reified.Aux[B, self.Mod, self.Fold] = new Reified[B] {

    override type Mod = self.Mod
    override type Fold[C, T] = self.Fold[C, T]

    override private[core] def modelComponent: Mod =
      self.modelComponent

    override def fold[C, T](b: B)(f: Folder[C, T]): Fold[C, T] =
      self.fold[C, T](g(b))(f)

    override def close[C, T](x: Fold[C, T], f: T => C): C =
      self.close(x, f)

    override def unfold[C, E, S](u: Unfolder[C, E, S])(c: C): Either[E, (B, C)] =
      self.unfold[C, E, S](u)(c).map { case (a, c) => (f(a), c) }
  }

  /**
   * Refine this `Reified` by restricting its domain
   *
   * @param r The representation of the refinement.
   * @param ev Evidence that the model type allows refining.
   */
  final def refined[B](r: Refinement.Aux[B, A])(
    implicit ev: Model.CanBeRefined[self.Mod]
  ): Reified.Aux[B, self.Mod, self.Fold] = new Reified[B] {

    override type Mod = self.Mod
    override type Fold[C, T] = self.Fold[C, T]

    override private[core] def modelComponent: Mod =
      ev.refine(self.modelComponent, r.semantics)

    override def fold[C, T](b: B)(f: Folder[C, T]): Fold[C, T] =
      self.fold[C, T](r.to(b))(f)

    override def close[C, T](x: Fold[C, T], f: T => C): C =
      self.close(x, f)

    override def unfold[C, E, S](u: Unfolder[C, E, S])(c: C): Either[E, (B, C)] = {
      self.unfold[C, E, S](u)(c).flatMap {
        case (a, c) =>
          r.from(a).map(a => (a, c)).leftMap(u.unknownError)
      }
    }
  }

  /**
   * Refine with only a UUID
   *
   * @see `refined`
   */
  final def pimap[B](id: UUID)(f: A => Either[String, B])(g: B => A)(
    implicit ev: Model.CanBeRefined[self.Mod]
  ): Reified.Aux[B, self.Mod, self.Fold] = {
    refined[B](new Refinement[B] {
      override type Repr = A
      override val uuid = id
      def from(r: Repr) = f(r)
      def to(b: B) = g(b)
    })(ev)
  }

  private[core] def unsafeWithDefaults(defs: List[Option[Any]]): Reified.Aux[A, this.Mod, this.Fold] =
    this
}

object Reified extends LowPrioReified1 {

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

  sealed abstract class AtomRepr {
    def stringRepr: String
    def binaryRepr: ByteVector
  }

  private final class AtomReprImpl[A](atom: A, atomic: Atomic[A])
      extends AtomRepr {
    final override def stringRepr: String =
      atomic.stringRepr(atom)
    final override def binaryRepr: ByteVector =
      atomic.binaryRepr(atom)
  }

  trait Folder[B, T] {
    def atom(repr: AtomRepr): B
    def hNil: T
    def hCons(l: Symbol, b: B, t: T): T
    def prod(t: T): B
    def sum(l: Symbol, b: B): B
    def vector(v: Vector[B]): B

    /** For ordering `B` elements (if necessary)
     *
     * The default implementation will order based on the
     * `CanonicalRepr` of the elements.
     */
    def orderB: Option[Order[B]] = None
  }

  object Folder {

    def instance[B, T](
      atom: AtomRepr => B,
      hNil: () => T,
      hCons: (Symbol, B, T) => T,
      prod: T => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B,
      orderB: Order[B] = null // scalastyle:ignore null
    ): Folder[B, T] = new FolderImpl[B, T](atom, hNil, hCons, prod, sum, vector, Option(orderB))

    def simple[B](
      atom: AtomRepr => B,
      hNil: () => B,
      hCons: (Symbol, B, B) => B,
      sum: (Symbol, B) => B,
      vector: Vector[B] => B,
      orderB: Order[B] = null // scalastyle:ignore null
    ): Folder[B, B] = instance[B, B](atom, hNil, hCons, identity, sum, vector, orderB)

    private final class FolderImpl[B, T](
        a: AtomRepr => B,
        hn: () => T,
        hc: (Symbol, B, T) => T,
        p: T => B,
        s: (Symbol, B) => B,
        vec: Vector[B] => B,
        ordB: Option[Order[B]]
    ) extends Folder[B, T] {
      override def atom(repr: AtomRepr): B = a(repr)
      override def hNil: T = hn()
      override def hCons(l: Symbol, b: B, t: T): T = hc(l, b, t)
      override def prod(t: T): B = p(t)
      override def sum(l: Symbol, b: B): B = s(l, b)
      override def vector(v: Vector[B]): B = vec(v)
      override val orderB = ordB
    }
  }

  sealed trait AtomResult[B]
  final case class StringResult[B](repr: String, rest: B)
    extends AtomResult[B]
  final case class BinaryResult[B](repr: ByteVector, inject: ByteVector => B)
    extends AtomResult[B]

  trait Unfolder[B, E, S] {
    def atom(b: B): Either[E, AtomResult[B]]
    def atomErr(b: B, err: Atomic.Error): E
    def hNil(b: B): Either[E, B]
    def hCons(b: B, l: Symbol): Either[E, Either[E, (B, B => Either[E, B])]]
    def cNil(b: B): E
    def cCons(b: B, l: Symbol): Either[E, Either[B, B]]
    def vectorInit(b: B): Either[E, (B, S)]
    def vectorFold(b: B, s: S): Either[E, Option[(B, S)]]
    def unknownError(msg: String): E
  }

  object Unfolder {

    def instance[B, E, S](
      atom: B => Either[E, AtomResult[B]],
      atomErr: (B, Atomic.Error) => E,
      hNil: B => Either[E, B],
      hCons: (B, Symbol) => Either[E, Either[E, (B, B => Either[E, B])]],
      cNil: B => E,
      cCons: (B, Symbol) => Either[E, Either[B, B]],
      vectorInit: B => Either[E, (B, S)],
      vectorFold: (B, S) => Either[E, Option[(B, S)]],
      unknownError: String => E
    ): Unfolder[B, E, S] = new UnfolderImpl[B, E, S](
      atom,
      atomErr,
      hNil,
      hCons,
      cNil,
      cCons,
      vectorInit,
      vectorFold,
      unknownError
    )

    private final class UnfolderImpl[B, E, S](
      a: B => Either[E, AtomResult[B]],
      ae: (B, Atomic.Error) => E,
      hn: B => Either[E, B],
      hc: (B, Symbol) => Either[E, Either[E, (B, B => Either[E, B])]],
      cn: B => E,
      cc: (B, Symbol) => Either[E, Either[B, B]],
      vi: B => Either[E, (B, S)],
      vf: (B, S) => Either[E, Option[(B, S)]],
      ue: String => E
    ) extends Unfolder[B, E, S] {
      override def atom(b: B): Either[E, AtomResult[B]] = a(b)
      override def atomErr(b: B, err: Atomic.Error): E = ae(b, err)
      override def hNil(b: B): Either[E, B] = hn(b)
      override def hCons(b: B, l: Symbol): Either[E, Either[E, (B, B => Either[E, B])]] = hc(b, l)
      override def cNil(b: B): E = cn(b)
      override def cCons(b: B, l: Symbol): Either[E, Either[B, B]] = cc(b, l)
      override def vectorInit(b: B): Either[E, (B, S)] = vi(b)
      override def vectorFold(b: B, s: S): Either[E, Option[(B, S)]] = vf(b, s)
      override def unknownError(msg: String): E = ue(msg)
    }
  }

  def apply[A](implicit d: Lazy[Reified[A]]): Aux[A, d.value.Mod, d.value.Fold] =
    d.value

  implicit val reifiedInvariantMonoidalFunctor: InvariantMonoidal[Reified] = new InvariantMonoidal[Reified] {

    type Fst = Witness.`'_1`.T
    type Snd = Witness.`'_2`.T

    override def unit: Reified[Unit] =
      point(())

    override def point[A](a: A): Reified[A] =
      Reified[HNil].imap(_ => a)(_ => HNil)

    override def imap[A, B](fa: Reified[A])(f: A => B)(g: B => A): Reified[B] =
      fa.imap(f)(g)

    override def product[A, B](fa: Reified[A], fb: Reified[B]): Reified[(A, B)] = {
      implicit val ra: Reified[A] = fa
      implicit val rb: Reified[B] = fb
      Reified[FieldType[Fst, A] :: FieldType[Snd, B] :: HNil].imap[(A, B)] { hl =>
        (hl.head, hl.tail.head)
      } { case (a, b) =>
        field[Fst](a) :: field[Snd](b) :: HNil
      }
    }
  }

  implicit def reifiedFromAtomic[A](
    implicit A: Atomic[A]
  ): Reified.Aux[A, Model.Atom, FFirst] = new Reified[A] {
    override type Mod = Model.Atom
    override type Fold[B, T] = B
    private[core] override val modelComponent = A.atom
    override def fold[B, T](a: A)(f: Folder[B, T]): B =
      f.atom(new AtomReprImpl(a, A))
    override def close[B, T](x: Fold[B, T], f: T => B): B =
      x
    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (A, B)] = {
      u.atom(b).flatMap { res: AtomResult[B] =>
        val e: Either[Atomic.Error, (A, B)] = res match {
          case StringResult(s, b) =>
            A.fromString(s).map(a => (a, b))
          case BinaryResult(bv, inj) =>
            A.fromBinary(bv).map { case (a, r) => (a, inj(r)) }
        }
        e.leftMap(err => u.atomErr(b, err))
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
    override def fold[B, T](a: F[A])(f: Folder[B, T]): Fold[B, T] = {
      f.vector(F.toVector(a).map(x => R.close(R.fold[B, T](x)(f), f.prod)))
    }
    override def close[B, T](x: Fold[B, T], f: T => B): B =
      x
    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (F[A], B)] =
      unfoldKleene[A, B, E, S](u)(b).map { case (vec, b) => (F.fromVector(vec), b) }
  }

  implicit def reifiedFromExtSet[F[_], A](
    implicit
    F: ExtSet[F, A],
    R: Reified[A]
  ): Reified.Aux[F[A], Model.Vector, FFirst] = {
    reifiedFromSetOrMap[F, A](isMap = false)(F, R)
  }

  implicit def reifiedFromExtMap[F[_, _], K, V](
    implicit
    F: ExtMap[F, K, V],
    K: Reified[K],
    V: Reified[V]
  ): Reified.Aux[F[K, V], Model.Vector, FFirst] = {
    type Synth[_] = F[K, V]
    val synthExtSet = new ExtSet[Synth, (K, V)] {
      def toVector(fa: F[K, V]): Vector[(K, V)] = F.toVector(fa)
      def fromVector(v: Vector[(K, V)]): Option[F[K, V]] = F.fromVector(v)
    }
    reifiedFromSetOrMap[Synth, (K, V)](isMap = true)(synthExtSet, Reified[(K, V)])
  }

  private def reifiedFromSetOrMap[F[_], A](isMap: Boolean)(
    implicit
    F: ExtSet[F, A],
    R: Reified[A]
  ): Reified.Aux[F[A], Model.Vector, FFirst] = {
    val semantics = if (isMap) Refinement.Semantics.map else Refinement.Semantics.set
    val errMsg = if (isMap) "duplicate keys" else "duplicate elements"
    new Reified[F[A]] {
      override type Mod = Model.Vector
      override type Fold[B, T] = B
      private[core] override val modelComponent =
        Model.Vector(R.modelComponent, Some(semantics))
      override def fold[B, T](fa: F[A])(f: Folder[B, T]): B = {
        val sortedBs: Vector[B] = f.orderB match {
          case Some(ordB) =>
            val bs = F.toVector(fa).map(x => R.close(R.fold[B, T](x)(f), f.prod))
            bs.sorted(ordB.toOrdering)
          case None =>
            val sortedAs = F.toVector(fa).sortWith { (x, y) =>
              val rx = R.foldClose(x)(CanonicalRepr.folder)
              val ry = R.foldClose(y)(CanonicalRepr.folder)
              CanonicalRepr.orderForCanonicalRepr.lt(rx, ry)
            }
            sortedAs.map(x => R.close(R.fold[B, T](x)(f), f.prod))
        }
        f.vector(sortedBs)
      }
      override def close[B, T](x: Fold[B, T], f: T => B): B =
        x
      override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (F[A], B)] = {
        unfoldKleene[A, B, E, S](u)(b).flatMap { case (vec, b) =>
          F.fromVector(vec).fold[Either[E, (F[A], B)]](Left(u.unknownError(errMsg))) { fa =>
            Right((fa, b))
          }
        }
      }
    }
  }

  private def unfoldKleene[A, B, E, S](u: Unfolder[B, E, S])(b: B)(
    implicit R: Reified[A]
  ): Either[E, (Vector[A], B)] = {
    u.vectorInit(b).flatMap { case (b, s) =>
      val rec = Monad[Either[E, ?]].tailRecM((b, s, Vector.empty[A])) { case (b, s, v) =>
        u.vectorFold(b, s).flatMap {
          case Some((b, s)) =>
            val b2 = R.unfold[B, E, S](u)(b)
            b2.map { case (a, b) =>
              Left((b, s, v :+ a))
            }
          case None =>
            Either.right(Right((b, s, v)))
        }
      }

      rec.map { case (b, _, v) =>
        (v, b)
      }
    }
  }

  private type SomeRepr[A] = Record.`'value -> A`.T
  private type OptionRepr[A] = Union.`'None -> HNil, 'Some -> SomeRepr[A]`.T

  // TODO: workaround for false positive unused warning
  locally { val _: SomeRepr[Int] = Record(value = 0) }

  /**
   * `Reified` instance for `Option[A]`.
   *
   * Necessary to be able to manually control the
   * model for Options, since the automatically
   * generated model is different for Scala 2.11
   * and 2.12 (due to the renaming of the field `x`
   * of `Some` to `value`).
   */
  implicit def reifiedForOption[A](
    implicit A: Lazy[Reified[A]]
  ): Reified.Aux[Option[A], Model.CCons, FFirst] = {

    Reified[OptionRepr[A]].imap[Option[A]] {
      case Inl(_) => None
      case Inr(Inl(r)) => Some(r('value))
      case Inr(Inr(cnil)) => cnil.impossible
    } {
      case None => Union[OptionRepr[A]](None = HNil : HNil)
      case Some(value) => Union[OptionRepr[A]](Some = Record(value = value))
    }
  }
}

/** Standard refinements */
private[core] sealed trait LowPrioReified1 extends LowPrioReified2 {

  import Reified.{ FFirst, FSecond }

  implicit lazy val reifiedForSymbol: Reified.Aux[Symbol, Model.Atom, FFirst] = {
    Reified
      .reifiedFromAtomic[String](Atomic.builtinString)
      .refined(new Refinement[Symbol] {
        override type Repr = String
        override val uuid = uuid"8c750487-1a6b-4c99-b01a-f1392b8177ed"
        override def repr = Refinement.ReprFormat.single("Symbol")
        override def from(s: String) = Right(Symbol(s))
        override def to(sym: Symbol) = sym.name
      })
  }

  /** Cached here to avoid always rematerializing */
  private[this] lazy val enumLikeForRoundingMode: EnumLike[RoundingMode] =
    EnumLike[RoundingMode]

  /** Uses the cached `EnumLike[RoundingMode]` */
  implicit lazy val reifiedForRoundingMode: Reified.Aux[RoundingMode, Model.Atom, FFirst] =
    Reified.reifiedForEnumLike[RoundingMode](enumLikeForRoundingMode)

  implicit def reifiedForShapelessNat[N <: Nat](
    implicit
    toInt: ToInt[N],
    wit: Witness.Aux[N]
  ): Reified.Aux[N, Model.Atom, FFirst] = {
    Reified
      .reifiedFromAtomic[Int](Atomic.builtinInt)
      .refined(new Refinement[N] {
        override type Repr = Int
        override val uuid =
          (uuid"f13a06d7-8442-4454-8b9e-9cc246428959" / Atomic.builtinInt.binaryRepr(toInt())).uuid
        override val repr =
          Refinement.ReprFormat.single(sh"shapeless.Nat(${toInt()})")
        override def from(i: Int): Either[String, N] =
          if (i === toInt()) Right(wit.value) else Left(sh"${i} is not ${toInt()}")
        override def to(n: N): Int =
          toInt()
      })
  }

  private type MathContextRepr = Record.`'precision -> Int, 'roundingMode -> RoundingMode`.T

  implicit lazy val reifiedForMathContext: Reified.Aux[MathContext, Model.HCons[Model.HCons[Model.HNil.type]], FSecond] = {
    Reified[MathContextRepr].refined(new Refinement[MathContext] {
      override type Repr = MathContextRepr
      override val uuid =
        uuid"6e099f51-bdc0-415b-8f73-bff72cfd47db"
      override val repr =
        Refinement.ReprFormat.single("java.math.MathContext")
      override def from(r: MathContextRepr): Either[String, MathContext] = {
        val precision: Int = r.head
        val roundingMode: RoundingMode = r.tail.head
        if (precision < 0) {
          Left(sh"negative precision: ${precision}")
        } else {
          Right(new MathContext(precision, roundingMode))
        }
      }
      override def to(mc: MathContext): MathContextRepr =
        Record(precision = mc.getPrecision, roundingMode = mc.getRoundingMode)
    })
  }

  private type BigDecimalRepr = Record.`'intVal -> BigInt, 'scale -> Int, 'ctx -> MathContext`.T

  implicit lazy val reifiedForBigDecimal: Reified.Aux[BigDecimal, Model.HCons[Model.HCons[Model.HCons[Model.HNil.type]]], FSecond] = {
    Reified[BigDecimalRepr].imap[BigDecimal] { r =>
      val ctx = r('ctx)
      new BigDecimal(new java.math.BigDecimal(r('intVal).bigInteger, r('scale), ctx), ctx)
    } { a =>
      Record(intVal = new BigInt(a.bigDecimal.unscaledValue), scale = a.bigDecimal.scale, ctx = a.mc)
    }
  }
}

private[core] sealed trait LowPrioReified2 extends LowPrioReified3 {

  import Reified.FFirst

  implicit def reifiedForEnumLike[A](implicit A: EnumLike[A]): Reified.Aux[A, Model.Atom, FFirst] = {
    Reified
      .reifiedFromAtomic[Int](Atomic.builtinInt)
      .refined(Refinement.enum[A](A))
  }
}

private[core] sealed trait LowPrioReified3 extends LowPrioReified4 {

  import Reified.{ FFirst, FSecond }

  implicit def reifiedFromHcons[HK <: Symbol, HV, T <: HList, MT <: Model.HList](
    implicit
    label: Witness.Aux[HK],
    rh0: Lazy[Reified[HV]],
    rt0: Lazy[Reified.Aux[T, MT, FSecond]]
  ): Reified.Aux[FieldType[HK, HV] :: T, Model.HCons[MT], FSecond] = {
    new HConsReified[HK, HV, T, MT](label, None, () => rh0, () => rt0)
  }

  private final class HConsReified[HK <: Symbol, HV, T <: HList, MT <: Model.HList](
    private[this] val label: Witness.Aux[HK],
    private[this] val headDefault: Option[HV],
    rh0: () => Lazy[Reified[HV]],
    rt0: () => Lazy[Reified.Aux[T, MT, FSecond]]
  )   extends WithLazy[Reified[HV], Reified.Aux[T, MT, FSecond]](rh0, rt0)
      with Reified[FieldType[HK, HV] :: T] {

    override type Mod = Model.HCons[MT]
    override type Fold[X, Y] = Y

    lazy private[core] override val modelComponent: Mod =
      Model.HCons(label.value, head.modelComponent, tail.modelComponent)

    private[core] override def unsafeWithDefaults(defs: List[Option[Any]]): Reified.Aux[FieldType[HK, HV] :: T, Model.HCons[MT], FSecond] = {
      new HConsReified[HK, HV, T, MT](
        label,
        defs.head.map(_.asInstanceOf[HV]),
        () => head,
        () => tail.unsafeWithDefaults(defs.tail)
      )
    }

    override def fold[B, U](a: FieldType[HK, HV] :: T)(f: Folder[B, U]): Fold[B, U] = {
      val h = head.close(head.fold(a.head)(f), f.prod)
      val t = tail.fold(a.tail)(f)
      f.hCons(label.value, h, t)
    }

    override def close[B, U](x: Fold[B, U], f: U => B): B =
      f(x)

    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (FieldType[HK, HV] :: T, B)] = {
      for {
        ht <- u.hCons(b, label.value).fold[Either[E, (HV, B)]](
          missingField => {
            Either.fromOption[E, HV](headDefault, missingField).map(h => (h, b))
          },
          ok => ok.flatMap { case (hb, f) =>
            head.unfold(u)(hb).flatMap {
              case (h, b) => f(b).map(b => (h, b))
            }
          }
        )
        (h, tb) = ht
        tb <- tail.unfold(u)(tb)
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
      override def fold[B, U](a: HNil)(f: Folder[B, U]): Fold[B, U] =
        f.hNil
      override def close[B, U](x: Fold[B, U], f: U => B): B =
        f(x)
      override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (HNil, B)] =
        u.hNil(b).map(b => (HNil, b))
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
    override def fold[B, U](a: FieldType[HK, HV] :+: T)(f: Folder[B, U]): Fold[B, U] = a match {
      case Inl(left) =>
        val l = head.fold[B, U](left)(f)
        f.sum(label.value, head.close(l, f.prod))
      case Inr(right) =>
        tail.close(tail.fold[B, U](right)(f), f.prod)
    }
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      x
    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (FieldType[HK, HV] :+: T, B)] = {
      for {
        e <- u.cCons(b, label.value)
        x <- e match {
          case Left(h) =>
            head.unfold(u)(h).map {
              case (h, b) => (Inl[FieldType[HK, HV], T](field[HK](h)), b)
            }
          case Right(t) =>
            tail.unfold(u)(t).map {
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
    override def fold[B, U](a: CNil)(f: Folder[B, U]): Fold[B, U] =
      a.impossible
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      x
    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (CNil, B)] =
      Either.left(u.cNil(b))
  }

  // FIXME: This is really similar to `reifiedFromGenericProductWithDefaults`,
  // FIXME: but doesn't seem to cause the same implicit ambiguities. Why?
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
    override def fold[B, U](a: A)(f: Folder[B, U]): Fold[B, U] = {
      head.fold(A.to(a))(f)
    }
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      RA.value.close(x, f)
    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (A, B)] =
      head.unfold(u)(b).map { case (ga, b) => (A.from(ga), b) }
  }
}

private[core] sealed trait LowPrioReified4 extends LowPrioReified5 {

  // TODO: if this is implicit, it causes problems (probably cycles)
  def reifiedFromRefinement[A, R, M <: Model, F[_, _]](
    implicit
    repr: Reified.Aux[R, M, F],
    refine: Refinement.Aux[A, R],
    ev: Model.CanBeRefined[M]
  ): Reified.Aux[A, M, F] = repr.refined[A](refine)(ev)
}

/** A trivial wrapper for `Reified` instances of case classes */
final case class Derived[A, M <: Model, FA[_, _]](instance: Reified.Aux[A, M, FA]) extends AnyVal

object Derived {

  /**
   * Provides a `Derived` instance for case classes
   *
   * A `Reified` instance will be provided based on
   * this by `LowPrioReified2#fromDerived`.
   */
  implicit def reifiedFromGenericProductWithDefaults[A, GA <: HList, DA <: HList, MA <: Model.HList, FA[_, _]](
    implicit
    A: LabelledGeneric.Aux[A, GA],
    D: Default.Aux[A, DA],
    DL: ToTraversable.Aux[DA, List, Option[Any]],
    WD: Model.WithDefaults[MA, DA],
    RA: Lazy[Reified.Aux[GA, MA, FA]]
  ): Derived[A, MA, FA] = {
    Derived[A, MA, FA](Reified.reifiedFromGenericProductWithDefaults[A, GA, DA, MA, FA])
  }
}

private[core] sealed trait LowPrioReified5 {

  /**
   * Provide a `Reified` instance from a `Derived` instance
   *
   * This is to allow lowering the priority of instances
   * derived through `Generic` for case classes.
   *
   * @see `Derived`
   */
  implicit def fromDerived[A, M <: Model, FA[_, _]](implicit d: Derived[A, M, FA]): Reified.Aux[A, M, FA] =
    d.instance

  /**
   * This is only invoked through `Derived`
   *
   * The reason for not making this implicit is that apparently that
   * would make it's priority the same as a `cachedImplicit` in a
   * companion object (thus causing ambiguous implicit errors).
   * This way, `Derived` has a lower priority, but still found if
   * needed.
   *
   * @see `Derived`
   * @see `LowPrioReified2#fromDerived`
   */
  def reifiedFromGenericProductWithDefaults[A, GA <: HList, DA <: HList, MA <: Model.HList, FA[_, _]](
    implicit
    A: LabelledGeneric.Aux[A, GA],
    D: Default.Aux[A, DA],
    DL: ToTraversable.Aux[DA, List, Option[Any]],
    WD: Model.WithDefaults[MA, DA],
    RA: Lazy[Reified.Aux[GA, MA, FA]]
  ): Reified.Aux[A, MA, FA] = new WithLazy[Reified[GA] { type Mod = MA; type Fold[X, Y] = FA[X, Y] }, None.type](
      () => RA.map(_.unsafeWithDefaults(D.apply().toList)),
      () => None
  ) with Reified[A] {
    override type Mod = MA
    override type Fold[B, U] = FA[B, U]
    lazy private[core] override val modelComponent: Mod = {
      WD.unsafeWithDefaults(head.modelComponent)(D())
    }
    override def fold[B, U](a: A)(f: Folder[B, U]): Fold[B, U] = {
      head.fold(A.to(a))(f)
    }
    override def close[B, U](x: Fold[B, U], f: U => B): B =
      RA.value.close(x, f)
    override def unfold[B, E, S](u: Unfolder[B, E, S])(b: B): Either[E, (A, B)] =
      head.unfold(u)(b).map { case (ga, b) => (A.from(ga), b) }
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

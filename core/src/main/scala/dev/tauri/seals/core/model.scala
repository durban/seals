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

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3

import cats.{ Eq, Show }
import cats.data.State
import cats.implicits._

// TODO: maybe rename HNil -> PNil and CNil -> SNil

sealed trait Model extends Serializable {

  import Model._

  lazy val desc: String = {
    this.fold[Desc](
      hNil = () => Desc.Leaf("HNil"),
      hCons = (l, o, r, h, t) => Desc.Branch("::", l, h, t, headPostfix = if (o) "?" else "").refined(r.map(_.desc)),
      cNil = () => Desc.Leaf("CNil"),
      cCons = (l, r, h, t) => Desc.Branch(":+:", l, h, t).refined(r.map(_.desc)),
      vector = (r, e) => e.withPostfix("*").refined(r.map(_.desc)),
      atom = a => Desc.Leaf(a.atomDesc),
      cycle = () => Desc.Leaf("<...>")
    ).toString
  }

  protected[core] def compEq(that: Model, compat: Boolean, memo: IdMemo): Boolean

  final def fold[R](
    hNil: () => R,
    hCons: (Symbol, Boolean, Option[Ref], R, R) => R,
    cNil: () => R,
    cCons: (Symbol, Option[Ref], R, R) => R,
    vector: (Option[Ref], R) => R,
    atom: Atom => R,
    cycle: () => R
  ): R = {
    foldImpl(
      _ => hNil(),
      (_, l, o, ref, h: R, t: R) => hCons(l, o, ref, h, t),
      _ => cNil(),
      (_, l, ref, h: R, t: R) => cCons(l, ref, h, t),
      (_, ref, e: R) => vector(ref, e),
      (_, a) => atom(a),
      _ => cycle(),
      Memo.valMemo,
      Path.empty
    )
  }

  final def foldC[R](
    hNil: Ctx => R,
    hCons: (Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
    cNil: Ctx => R,
    cCons: (Ctx, Symbol, Option[Ref], R, R) => R,
    vector: (Ctx, Option[Ref], R) => R,
    atom: (Ctx, Atom) => R,
    cycle: Ctx => R
  ): R = foldImpl(hNil, hCons, cNil, cCons, vector, atom, cycle, Memo.valMemo, Path.empty)

  protected[core] def foldImpl[R](
    hNil: Ctx => R,
    hCons: (Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
    cNil: Ctx => R,
    cCons: (Ctx, Symbol, Option[Ref], R, R) => R,
    vector: (Ctx, Option[Ref], R) => R,
    atom: (Ctx, Atom) => R,
    cycle: Ctx => R,
    memo: Memo,
    path: Path
  ): R

  final def compatible(that: Model): Boolean =
    this.compEq(that, compat = true, memo = Memo.idMemo)

  final override def equals(that: Any): Boolean = that match {
    case that: Model =>
      if (this eq that) true
      else this.compEq(that, compat = false, memo = Memo.idMemo)
    case _ =>
      false
  }

  final override def hashCode: Int = {
    val st = this.foldImpl[State[(Int, Int), Unit]](
      hNil = _ => hash.mix(hash.hNil),
      hCons = (_, l, o, r, h, t) => hash.mix(o) *> hash.mix(r.map(_.uuid)) *> hash.mixLht(l, h, t, hash.hCons),
      cNil = _ => hash.mix(hash.cNil),
      cCons = (_, l, r, h, t) => hash.mix(r.map(_.uuid)) *> hash.mixLht(l, h, t, hash.cCons),
      vector = (_, r, e) => hash.mix(hash.vector) *> hash.mix(r.map(_.uuid)) *> e,
      atom = (_, a) => hash.mix(a.atomHash),
      cycle = _ => State.pure(()),
      memo = Memo.valMemo,
      path = Path.empty
    )
    val (state, length) = st.runS((hash.seed, 0)).value
    assert(length > 0)
    MurmurHash3.finalizeHash(state, length)
  }

  final override def toString: String =
    sh"Model[${this.desc}]"

  final def pathComp: String = this match {
    case HNil => "HNil"
    case _: HCons[_] => HCons.pathComp
    case CNil => "CNil"
    case _: CCons => CCons.pathComp
    case _: Atom => Atom.pathComp
    case _: Vector => "Vector"
  }

  final def paths: Map[Model, Path] =
    cachedPaths

  @transient
  private[this] lazy val cachedPaths =
    cachedPathsAndIds.mapValues(_._1)

  final def localIds: Map[Model, Int] =
    cachedIds

  @transient
  private[this] lazy val cachedIds =
    cachedPathsAndIds.mapValues(_._2)

  private[this] type MapP = (Map[Model, (Model.Path, Int)], Int)
  private[this] type StMapP = State[MapP, Unit]

  @transient
  private[this] lazy val cachedPathsAndIds: Map[Model, (Model.Path, Int)] = {
    def compositePre(
      label: String,
      c: Model.Ctx,
      l: Symbol,
      h: StMapP,
      t: StMapP
    ): StMapP = {
      for {
        _ <- h
        _ <- t
        _ <- State.modify[MapP] { case (map, ctr) =>
          (map + (c.m -> ((c.p, ctr))), ctr + 1)
        }
      } yield ()
    }
    val st = this.foldC[StMapP](
        hNil = _ => State.pure(()),
        hCons = (c, l, _, _, h, t) => compositePre("HCons", c, l, h, t),
        cNil = _ => State.pure(()),
        cCons = (c, l, _, h, t) => compositePre("CCons", c, l, h, t),
        vector = (c, _, e) => e,
        atom = (_, a) => State.pure(()),
        cycle = _ => State.pure(())
    )
    st.runS((Map.empty, 0)).value._1
  }
}

object Model {

  sealed trait CanBeRefined[M <: Model] extends Serializable {
    def refine(m: M, r: Refinement.Semantics): M
  }

  object CanBeRefined {

    def apply[M <: Model](implicit cbr: CanBeRefined[M]): CanBeRefined[M] =
      cbr

    def combine(r1: UUID, r2: UUID): UUID =
      NsUUID.uuid5nestedNs(r1, r2) // FIXME

    def combine(r1: Option[Ref], r2: Ref): Some[Ref] =
      Some(r1.fold(r2)(r1 => Ref(combine(r1.uuid, r2.uuid), r1.repr combine r2.repr)))

    implicit val atomCanBeRefined: CanBeRefined[Model.Atom] = new CanBeRefined[Model.Atom] {
      override def refine(m: Model.Atom, r: Refinement.Semantics) =
        m.refined(r)
    }

    implicit val cConsCanBeRefined: CanBeRefined[Model.CCons] = new CanBeRefined[Model.CCons] {
      override def refine(m: Model.CCons, r: Refinement.Semantics) =
        m.refined(r)
    }

    implicit def hConsCanBeRefined[T <: Model.HList]: CanBeRefined[Model.HCons[T]] = new CanBeRefined[Model.HCons[T]] {
      override def refine(m: Model.HCons[T], r: Refinement.Semantics) =
        m.refined(r)
    }

    implicit val vectorCanBeRefined: CanBeRefined[Model.Vector] = new CanBeRefined[Model.Vector] {
      override def refine(m: Model.Vector, r: Refinement.Semantics) =
        m.refined(r)
    }
  }

  final case class Ctx(m: Model, p: Path)

  type Path = scala.Vector[String]
  val Path = scala.Vector

  private[core] type Memo = Memo.Memo[Model]

  private[core] type IdMemo = Memo.IdMemo[Model]

  private[core] type Ref = Refinement.Semantics
  private[core] val Ref = Refinement.Semantics

  implicit val modelEquality: Eq[Model] =
    Eq.fromUniversalEquals[Model]

  implicit val modelShow: Show[Model] =
    Show.fromToString[Model]

  /**
   * Reified instance for Model
   */
  implicit val reifiedForModel: Reified.Aux[Model, Model.CCons, Reified.FFirst] =
    ModelRepr.reifiedForModelRepr.refined[Model](ModelRepr.modelRefinement)

  private object hash {

    final val seed = MurmurHash3.traversableSeed

    final val hNil = 0x633f31ae
    final val hCons = 0x47d21c6c
    final val cNil = 0x23ae37b5
    final val cCons = 0xae125fa4
    final val vector = 0x6e9f217b

    final val `true` = 0x2460f70d
    final val `false` = 0x1312ff7f

    final def mix(b: Boolean): State[(Int, Int), Unit] =
      mix(if (b) `true` else `false`)

    final def mix(r: Option[UUID]): State[(Int, Int), Unit] =
      r.fold(State.pure[(Int, Int), Unit](()))(u => mix(u.##))

    final def mix(mix: Int): State[(Int, Int), Unit] = {
      for {
        sl <- State.get[(Int, Int)]
        (state, length) = sl
        mixed = mixHash(state, mix)
        _ <- State.set((mixed, length + 1))
      } yield ()
    }

    final def mixLht(
      l: Symbol,
      h: State[(Int, Int), Unit],
      t: State[(Int, Int), Unit],
      m: Int
    ): State[(Int, Int), Unit] = {
      for {
        _ <- mix(l.##)
        _ <- h
        _ <- t
        _ <- mix(m)
      } yield ()
    }

    final def mixHash(state: Int, mix: Int): Int =
      MurmurHash3.mix(state, mix)
  }

  private sealed trait Desc {
    def withPostfix(postfix: String): Desc
    def map(f: String => String): Desc
    def refined(r: Option[String => String]): Desc =
      r.fold(this)(d => this.map(d))
  }

  private object Desc {

    implicit val showDesc: Show[Desc] =
      Show.fromToString[Desc]

    final case class Leaf(label: String, postfix: String = "") extends Desc {
      override def toString = label + postfix
      override def withPostfix(postfix: String): Leaf = this.copy(postfix = postfix)
      override def map(f: String => String): Desc = Mapped(this, f)
    }

    final case class Branch(
      label: String,
      l: Symbol,
      h: Desc,
      t: Desc,
      headPostfix: String = "",
      postfix: String = ""
    ) extends Desc {

      override def toString = {
        val repr = h match {
          case Branch(_, _, _, _, _, _) | Mapped(Branch(_, _, _, _, _, _), _) =>
            sh"${l} -> (${h}${headPostfix}) ${label} ${t}"
          case _ =>
            sh"${l} -> ${h}${headPostfix} ${label} ${t}"
        }
        if (postfix.nonEmpty) {
          sh"(${repr})${postfix}"
        } else {
          repr
        }
      }

      override def withPostfix(postfix: String): Branch =
        this.copy(postfix = postfix)

      override def map(f: String => String): Desc =
        Mapped(this, r => f(sh"(${r})"))
    }

    final case class Mapped(d: Desc, f: String => String) extends Desc {

      override def toString =
        f(d.toString)

      override def withPostfix(postfix: String): Mapped =
        Mapped(d, r => sh"(${f(r)})${postfix}")

      override def map(g: String => String): Desc =
        Mapped(d, r => g(sh"(${f(r)})"))
    }
  }

  sealed abstract class Composite[+H <: Model, +T <: Model](
      val label: Symbol,
      private[this] var h: () => H,
      private[this] var t: () => T
  ) extends Model {
    final lazy val head: H = {
      val res = h()
      // clean the closure to
      // avoid capturing anything
      // longer than necessary:
      h = null // scalastyle:ignore
      res
    }
    final lazy val tail: T = {
      val res = t()
      // clean the closure to
      // avoid capturing anything
      // longer than necessary:
      t = null // scalastyle:ignore
      res
    }
    protected def checkType(that: Model): Option[Composite[Model, Model]]
    private[Model] def refinement: Option[Ref]
    protected[core] override def compEq(that: Model, compat: Boolean, memo: IdMemo) = {
      this.checkType(that).map { that =>
        if (memo contains that) {
          true
        } else {
          if ((this.label === that.label) && (this.refinement === that.refinement)) {
            val newMemo = memo + that
            this.head.compEq(that.head, compat, newMemo) &&
            this.tail.compEq(that.tail, compat, newMemo)
          } else {
            false
          }
        }
      }.getOrElse(false)
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

  sealed trait Sentinel extends Model

  sealed trait WithDefaults[M <: HList, D <: shapeless.HList] extends Serializable {
    def unsafeWithDefaults(m: M)(d: D): M
  }

  object WithDefaults {

    implicit val hNilWithDefaults: WithDefaults[HNil.type, shapeless.HNil] = {
      new WithDefaults[HNil.type, shapeless.HNil] {
        override def unsafeWithDefaults(m: HNil.type)(d: shapeless.HNil): HNil.type =
          HNil
      }
    }

    implicit def hConsWithDefaults[H <: Option[Any], T <: shapeless.HList, MT <: HList](
      implicit t: WithDefaults[MT, T]
    ): WithDefaults[HCons[MT], shapeless.::[H, T]] = {
      new WithDefaults[HCons[MT], shapeless.::[H, T]] {
        override def unsafeWithDefaults(m: HCons[MT])(d: shapeless.::[H, T]): HCons[MT] = {
          val h = if (d.head.isDefined) {
            m.withDefault
          } else {
            m.withoutDefault
          }
          h.withTail(t.unsafeWithDefaults(h.tail)(d.tail))
        }
      }
    }
  }

  sealed trait HList extends Model {

    @tailrec
    private[core] final def foldLeft[B](z: B)(f: (B, Boolean) => B): B = this match {
      case _: HNil.type =>
        z
      case hc: HCons[_] =>
        (hc.tail : HList).foldLeft(f(z, hc.optional))(f)
    }
  }

  final case object HNil extends HList with Sentinel {
    def ::(h: (Symbol, Model)): HCons[HNil.type] =
      HCons(h._1, h._2, this)
    protected[core] override def compEq(that: Model, compat: Boolean, memo: IdMemo) = {
      if (compat) {
        that match {
          case hl: HList =>
            hl.foldLeft(true) { (st, opt) =>
              st && opt
            }
          case _ =>
            false
        }
      } else {
        this eq that
      }
    }
    protected[core] final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, Option[Ref], R, R) => R,
      vector: (Ctx, Option[Ref], R) => R,
      atom: (Ctx, Atom) => R,
      cycle: Ctx => R,
      memo: Memo,
      path: Path
    ): R = hNil(Ctx(this, path :+ this.pathComp))
  }

  final class HCons[T <: HList](
      l: Symbol,
      val optional: Boolean,
      h0: () => Model,
      t0: () => T,
      private[Model] val refinement: Option[Ref]
  ) extends Composite[Model, T](l, h0, t0) with HList {
    def ::(h: (Symbol, Model)): HCons[HCons[T]] =
      HCons(h._1, h._2, this)
    private[Model] def refined(r: Ref): HCons[T] =
      new HCons[T](l, optional, () => head, () => tail, CanBeRefined.combine(refinement, r))
    protected override def checkType(that: Model) = that match {
      case that: HCons[_] if this.optional === that.optional =>
        Some(that)
      case _ =>
        None
    }
    protected[core] override def compEq(that: Model, compat: Boolean, memo: IdMemo) = {
      if (compat) {
        that match {
          case HNil =>
            // we might still be compatible,
            // HNil has the logic for that:
            that.compEq(this, compat, memo)
          case _ =>
            super.compEq(that, compat, memo)
        }
      } else {
        super.compEq(that, compat, memo)
      }
    }
    protected[core] final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, Option[Ref], R, R) => R,
      vector: (Ctx, Option[Ref], R) => R,
      atom: (Ctx, Atom) => R,
      cycle: Ctx => R,
      memo: Memo,
      path: Path
    ): R = {
      if (memo contains this) {
        cycle(Ctx(this, path))
      } else {
        val newMemo = memo + this
        hCons(
          Ctx(this, path :+ this.pathComp),
          label,
          optional,
          refinement,
          head.foldImpl(
            hNil,
            hCons,
            cNil,
            cCons,
            vector,
            atom,
            cycle,
            newMemo,
            path :+ this.pathComp :+ "head"
          ),
          tail.foldImpl(
            hNil,
            hCons,
            cNil,
            cCons,
            vector,
            atom,
            cycle,
            newMemo,
            path :+ this.pathComp :+ "tail"
          )
        )
      }
    }

    private[core] def withDefault: HCons[T] =
      HCons(this.label, true, this.head, this.tail)

    private[core] def withoutDefault: HCons[T] =
      HCons(this.label, false, this.head, this.tail)

    private[core] def withTail(newTail: => T): HCons[T] =
      HCons(this.label, this.optional, this.head, newTail)
  }

  object HCons {
    private[core] val pathComp = "HCons"
    private[seals] def apply[T <: HList](label: Symbol, h: => Model, t: => T): HCons[T] =
      apply(label, false, None, h, t)
    private[seals] def apply[T <: HList](label: Symbol, optional: Boolean, h: => Model, t: => T): HCons[T] =
      apply(label, optional, None, h, t)
    private[seals] def apply[T <: HList](label: Symbol, optional: Boolean, refinement: Option[Ref], h: => Model, t: => T): HCons[T] =
      new HCons(label, optional, h _, t _, refinement)
  }

  sealed trait Coproduct extends Model

  final case object CNil extends Coproduct with Sentinel {
    def :+:(h: (Symbol, Model)): CCons =
      CCons(h._1, h._2, this)
    protected[core] override def compEq(that: Model, compat: Boolean, memo: IdMemo) =
      this eq that
    protected[core] final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, Option[Ref], R, R) => R,
      vector: (Ctx, Option[Ref], R) => R,
      atom: (Ctx, Atom) => R,
      cycle: Ctx => R,
      memo: Memo,
      path: Path
    ): R = cNil(Ctx(this, path :+ this.pathComp))
  }

  final class CCons(
      l: Symbol,
      h0: () => Model,
      t0: () => Coproduct,
      private[core] val refinement: Option[Ref]
  ) extends Composite[Model, Coproduct](l, h0, t0) with Coproduct {
    def :+:(h: (Symbol, Model)): CCons =
      CCons(h._1, h._2, this)
    private[Model] def refined(r: Ref): CCons =
      new CCons(l, () => head, () => tail, CanBeRefined.combine(refinement, r))
    protected override def checkType(that: Model) = that match {
      case that: CCons => Some(that)
      case _ => None
    }
    protected[core] final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, Option[Ref], R, R) => R,
      vector: (Ctx, Option[Ref], R) => R,
      atom: (Ctx, Atom) => R,
      cycle: Ctx => R,
      memo: Memo,
      path: Path
    ): R = {
      if (memo contains this) {
        cycle(Ctx(this, path))
      } else {
        val newMemo = memo + this
        cCons(
          Ctx(this, path :+ this.pathComp),
          label,
          refinement,
          head.foldImpl(
            hNil,
            hCons,
            cNil,
            cCons,
            vector,
            atom,
            cycle,
            newMemo,
            path :+ this.pathComp :+ "head"
          ),
          tail.foldImpl(
            hNil,
            hCons,
            cNil,
            cCons,
            vector,
            atom,
            cycle,
            newMemo,
            path :+ this.pathComp :+ "tail"
          )
        )
      }
    }
  }

  object CCons {
    private[core] val pathComp = "CCons"
    private[seals] def apply(label: Symbol, h: => Model, t: => Coproduct): CCons =
      apply(label, None, h, t)
    private[seals] def apply(label: Symbol, refinement: Option[Ref], h: => Model, t: => Coproduct): CCons =
      new CCons(label, h _, t _, refinement)
  }

  final class Atom private (
      private[seals] val uuid: UUID,
      private[core] val atomDesc: String
  ) extends Model {

    private[Model] def refined(r: Ref): Atom =
      new Atom(CanBeRefined.combine(uuid, r.uuid), r.desc(atomDesc))

    private[core] def atomHash: Int =
      this.uuid.##

    protected[core] final override def compEq(that: Model, compat: Boolean, memo: Model.IdMemo): Boolean = {
      that match {
        case that: Atom =>
          this.uuid === that.uuid
        case _ =>
          false
      }
    }

    protected[core] final override def foldImpl[R](
      hNil: Model.Ctx => R,
      hCons: (Model.Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
      cNil: Model.Ctx => R,
      cCons: (Model.Ctx, Symbol, Option[Ref], R, R) => R,
      vector: (Model.Ctx, Option[Ref], R) => R,
      atom: (Model.Ctx, Atom) => R,
      cycle: Model.Ctx => R,
      memo: Model.Memo,
      path: Model.Path
    ): R = {
      atom(Model.Ctx(this, path :+ this.pathComp), this)
    }
  }

  object Atom {

    private[core] val pathComp = "Atom"

    def apply(uuid: UUID, desc: String): Atom =
      new Atom(uuid, desc)

    def unknown(uuid: UUID): Atom =
      new Atom(uuid, sh"<UnknownAtom:${uuid}>")

    def atom[A](implicit atomic: Atomic[A]): Atom =
      atomic.atom

    implicit val eqForAtom: Eq[Atom] =
      Eq.fromUniversalEquals
  }

  final class Vector private (val elem: Model, private[Model] val refinement: Option[Ref])
      extends Model {

    private[Model] def refined(r: Ref): Vector =
      new Vector(elem, CanBeRefined.combine(refinement, r))

    def compEq(that: Model, compat: Boolean, memo: Model.IdMemo): Boolean = that match {
      case that: Vector =>
        (this.refinement === that.refinement) && this.elem.compEq(that.elem, compat, memo)
      case _ =>
        false
    }

    def foldImpl[R](
      hNil: Model.Ctx => R,
      hCons: (Model.Ctx, Symbol, Boolean, Option[Ref], R, R) => R,
      cNil: Model.Ctx => R,
      cCons: (Model.Ctx, Symbol, Option[Ref], R, R) => R,
      vector: (Model.Ctx, Option[Ref], R) => R,
      atom: (Model.Ctx, Atom) => R,
      cycle: Model.Ctx => R,
      memo: Model.Memo,
      path: Model.Path): R = vector(
      Model.Ctx(this, path :+ this.pathComp),
      refinement,
      elem.foldImpl(
        hNil,
        hCons,
        cNil,
        cCons,
        vector,
        atom,
        cycle,
        memo,
        path :+ this.pathComp :+ "elem"
      )
    )
  }

  object Vector {
    def apply(elem: Model, ref: Option[Ref] = None): Vector =
      new Vector(elem, ref)
  }
}

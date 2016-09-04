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

import scala.annotation.tailrec
import scala.util.hashing.MurmurHash3
import java.util.UUID
import cats.Eq
import cats.data.{ State, Xor }
import cats.implicits._

// TODO: maybe rename HNil -> PNil and CNil -> SNil
// FIXME: do we need to extend `Product`?

sealed trait Model extends Product with Serializable with AtomRegistry {

  import Model._

  lazy val desc: String = {
    this.fold[Desc](
      hNil = () => Desc.Leaf("HNil"),
      hCons = (l, o, h, t) => Desc.Branch("::", l, h, t, headPostfix = if (o) "?" else ""),
      cNil = () => Desc.Leaf("CNil"),
      cCons = (l, h, t) => Desc.Branch(":+:", l, h, t),
      atom = a => Desc.Leaf(a.atomDesc),
      cycle = () => Desc.Leaf("<...>")
    ).toString
  }

  protected def compEq(that: Model, compat: Boolean, memo: IdMemo): Boolean

  final def fold[R](
    hNil: () => R,
    hCons: (Symbol, Boolean, R, R) => R,
    cNil: () => R,
    cCons: (Symbol, R, R) => R,
    atom: Atom[_] => R,
    cycle: () => R
  ): R = {
    foldImpl(
      _ => hNil(),
      (_, l, o, h: R, t: R) => hCons(l, o, h, t),
      _ => cNil(),
      (_, l, h: R, t: R) => cCons(l, h, t),
      (_, a) => atom(a),
      _ => cycle(),
      Memo.valMemo,
      Path.empty
    )
  }

  final def foldC[R](
    hNil: Ctx => R,
    hCons: (Ctx, Symbol, Boolean, R, R) => R,
    cNil: Ctx => R,
    cCons: (Ctx, Symbol, R, R) => R,
    atom: (Ctx, Atom[_]) => R,
    cycle: Ctx => R
  ): R = foldImpl(hNil, hCons, cNil, cCons, atom, cycle, Memo.valMemo, Path.empty)

  protected def foldImpl[R](
    hNil: Ctx => R,
    hCons: (Ctx, Symbol, Boolean, R, R) => R,
    cNil: Ctx => R,
    cCons: (Ctx, Symbol, R, R) => R,
    atom: (Ctx, Atom[_]) => R,
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
      hCons = (_, l, o, h, t) => hash.mix(o).flatMap(_ => hash.mixLht(l, h, t, hash.hCons)),
      cNil = _ => hash.mix(hash.cNil),
      cCons = (_, l, h, t) => hash.mixLht(l, h, t, hash.cCons),
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
    s"Model[${this.desc}]"

  final override def canEqual(that: Any): Boolean = that match {
    case _: Model => true
    case _ => false
  }

  final def pathComp: String = this match {
    case HNil => "HNil"
    case _: HCons => HCons.pathComp
    case CNil => "CNil"
    case _: CCons => CCons.pathComp
    case _: Atom[_] => Atom.pathComp
  }

  final override def getAtom(id: UUID): Xor[String, Atom[_]] =
    Xor.fromOption(allAtoms.get(id), s"not found Atom with id ${id}")

  private[this] lazy val allAtoms: Map[UUID, Atom[_]] = {
    this.fold[Map[UUID, Atom[_]]](
      hNil = () => Map.empty,
      hCons = (_, _, h, t) => h ++ t,
      cNil = () => Map.empty,
      cCons = (_, h, t) => h ++ t,
      atom = a => Map(a.uuid -> a),
      cycle = () => Map.empty
    )
  }
}

object Model {

  final case class Ctx(m: Model, p: Path)

  type Path = Vector[String]
  val Path = Vector

  private[core] type Memo = Memo.Memo[Model]

  private[core] type IdMemo = Memo.IdMemo[Model]

  implicit val modelEquality: Eq[Model] =
    Eq.fromUniversalEquals[Model]

  private object hash {

    final val seed = MurmurHash3.traversableSeed

    final val hNil = 0x633f31ae
    final val hCons = 0x47d21c6c
    final val cNil = 0x23ae37b5
    final val cCons = 0xae125fa4

    final val `true` = 0x2460f70d
    final val `false` = 0x1312ff7f

    final def mix(b: Boolean): State[(Int, Int), Unit] =
      mix(if (b) `true` else `false`)

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

  private sealed trait Desc
  private object Desc {
    final case class Leaf(label: String) extends Desc {
      override def toString = label
    }
    final case class Branch(
      label: String,
      l: Symbol,
      h: Desc,
      t: Desc,
      headPostfix: String = ""
    ) extends Desc {
      override def toString = h match {
        case Branch(_, _, _, _, _) =>
          s"${l} -> (${h}${headPostfix}) ${label} ${t}"
        case _ =>
          s"${l} -> ${h}${headPostfix} ${label} ${t}"
      }
    }
  }

  sealed abstract class Composite[H <: Model, T <: Model](
      val label: Symbol,
      private[this] var h: () => H,
      private[this] var t: () => T
  ) extends Model {
    final lazy val head: H = {
      val res = h()
      // clean the closure to
      // avoid capturing anything
      // longer than necessary:
      h = null
      res
    }
    final lazy val tail: T = {
      val res = t()
      // clean the closure to
      // avoid capturing anything
      // longer than necessary:
      t = null
      res
    }
    protected def checkType(that: Model): Option[Composite[H, T]]
    protected override def compEq(that: Model, compat: Boolean, memo: IdMemo) = {
      this.checkType(that).map { that =>
        if (memo contains that) {
          true
        } else {
          if (this.label === that.label) {
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
    // FIXME: should we include `optional`?
    final override def productArity: Int = 3
    final override def productElement(n: Int): Any = n match {
      case 0 => label
      case 1 => head
      case 2 => tail
      case _ => throw new IndexOutOfBoundsException
    }
  }

  sealed trait Sentinel extends Model

  sealed trait WithDefaults[D <: shapeless.HList] extends Serializable {
    def unsafeWithDefaults(m: HList)(d: D): HList
  }

  object WithDefaults {

    implicit val hNilWithDefaults: WithDefaults[shapeless.HNil] = {
      new WithDefaults[shapeless.HNil] {
        override def unsafeWithDefaults(m: HList)(d: shapeless.HNil): HList =
          HNil
      }
    }

    implicit def hConsWithDefaults[H <: Option[Any], T <: shapeless.HList](
      implicit t: WithDefaults[T]
    ): WithDefaults[shapeless.::[H, T]] = {
      new WithDefaults[shapeless.::[H, T]] {
        override def unsafeWithDefaults(m: HList)(d: shapeless.::[H, T]): HList = {
          m match {
            case hc: HCons =>
              val h = if (d.head.isDefined) {
                hc.withDefault
              } else {
                hc.withoutDefault
              }
              h.withTail(t.unsafeWithDefaults(h.tail)(d.tail))
            case _ =>
              impossible("Model and Defaults length mismatch")
          }
        }
      }
    }
  }

  sealed trait HList extends Model {

    @tailrec
    private[core] final def foldLeft[B](z: B)(f: (B, Boolean) => B): B = this match {
      case _: HNil.type =>
        z
      case hc: HCons =>
        hc.tail.foldLeft(f(z, hc.optional))(f)
    }
  }

  final case object HNil extends HList with Sentinel {
    def ::(h: (Symbol, Model)): HCons =
      HCons(h._1, h._2, this)
    protected override def compEq(that: Model, compat: Boolean, memo: IdMemo) = {
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
    protected final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, R, R) => R,
      atom: (Ctx, Atom[_]) => R,
      cycle: Ctx => R,
      memo: Memo,
      path: Path
    ): R = hNil(Ctx(this, path :+ this.pathComp))
  }

  final class HCons(
      l: Symbol,
      val optional: Boolean,
      h0: () => Model,
      t0: () => HList
  ) extends Composite[Model, HList](l, h0, t0) with HList {
    def ::(h: (Symbol, Model)): HCons =
      HCons(h._1, h._2, this)
    protected override def checkType(that: Model) = that match {
      case that: HCons if this.optional === that.optional =>
        Some(that)
      case _ =>
        None
    }
    protected override def compEq(that: Model, compat: Boolean, memo: IdMemo) = {
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
    protected final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, R, R) => R,
      atom: (Ctx, Atom[_]) => R,
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
          head.foldImpl(
            hNil,
            hCons,
            cNil,
            cCons,
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
            atom,
            cycle,
            newMemo,
            path :+ this.pathComp :+ "tail")
        )
      }
    }

    private[core] def withDefault: HCons =
      HCons(this.label, true, this.head, this.tail)

    private[core] def withoutDefault: HCons =
      HCons(this.label, false, this.head, this.tail)

    private[core] def withTail(newTail: => HList): HCons =
      HCons(this.label, this.optional, this.head, newTail)
  }

  object HCons {
    private[core] val pathComp = "HCons"
    private[seals] def apply(label: Symbol, h: => Model, t: => HList): HCons =
      apply(label, false, h, t)
    private[seals] def apply(label: Symbol, optional: Boolean, h: => Model, t: => HList): HCons =
      new HCons(label, optional, h _, t _)
  }

  sealed trait Coproduct extends Model

  final case object CNil extends Coproduct with Sentinel {
    def :+:(h: (Symbol, Model)): CCons =
      CCons(h._1, h._2, this)
    protected override def compEq(that: Model, compat: Boolean, memo: IdMemo) =
      this eq that
    protected final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, R, R) => R,
      atom: (Ctx, Atom[_]) => R,
      cycle: Ctx => R,
      memo: Memo,
      path: Path
    ): R = cNil(Ctx(this, path :+ this.pathComp))
  }

  final class CCons(
      l: Symbol,
      h0: () => Model,
      t0: () => Coproduct
  ) extends Composite[Model, Coproduct](l, h0, t0) with Coproduct {
    def :+:(h: (Symbol, Model)): CCons =
      CCons(h._1, h._2, this)
    protected override def checkType(that: Model) = that match {
      case that: CCons => Some(that)
      case _ => None
    }
    protected final override def foldImpl[R](
      hNil: Ctx => R,
      hCons: (Ctx, Symbol, Boolean, R, R) => R,
      cNil: Ctx => R,
      cCons: (Ctx, Symbol, R, R) => R,
      atom: (Ctx, Atom[_]) => R,
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
          head.foldImpl(
            hNil,
            hCons,
            cNil,
            cCons,
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
      new CCons(label, h _, t _)
  }
}

sealed trait Atom[A] extends Model {

  def stringRepr(a: A): String

  def fromString(s: String): Option[A]

  protected def compEq(that: Atom[_]): Boolean

  private[core] def atomHash: Int

  private[core] def atomDesc: String

  private[seals] def uuid: UUID

  final protected override def compEq(that: Model, compat: Boolean, memo: Model.IdMemo): Boolean = {
    that match {
      case that: Atom[_] =>
        this.compEq(that)
      case _ =>
        false
    }
  }

  final override def productArity: Int = 0
  final override def productElement(n: Int): Any =
    throw new IndexOutOfBoundsException
}

object Atom {

  private[core] val pathComp = "Atom"

  def apply[A](implicit A: Atom[A]): Atom[A] = A

  implicit def builtinAtom[A](implicit builtin: BuiltinAtom[A]): Atom[A] =
    builtin.atom

  implicit def atomicAtom[A](implicit atomic: Atomic[A]): Atom[A] =
    new AtomicAtom[A](atomic)
}

private sealed abstract class AbstractAtom[A] extends Atom[A] {
  protected final override def foldImpl[R](
    hNil: Model.Ctx => R,
    hCons: (Model.Ctx, Symbol, Boolean, R, R) => R,
    cNil: Model.Ctx => R,
    cCons: (Model.Ctx, Symbol, R, R) => R,
    atom: (Model.Ctx, Atom[_]) => R,
    cycle: Model.Ctx => R,
    memo: Model.Memo,
    path: Model.Path): R = {
    atom(Model.Ctx(this, path :+ this.pathComp), this)
  }
}

private abstract class SimpleAtom[A](
    private[core] override val atomDesc: String,
    private[seals] override val uuid: UUID) extends AbstractAtom[A] {
  final protected override def compEq(that: Atom[_]) =
    this eq that
  private[core] final override def atomHash =
    System.identityHashCode(this)
}

private final class AtomicAtom[A](private val atomic: Atomic[A])
    extends AbstractAtom[A] {
  private[core] override def atomDesc: String = atomic.description
  private[core] override def atomHash: Int = atomic.uuid.##
  private[seals] override def uuid: UUID = atomic.uuid
  protected override def compEq(that: Atom[_]): Boolean = that match {
    case that: AtomicAtom[_] =>
      atomic.uuid === that.atomic.uuid
    case _ =>
      false
  }
  override def fromString(s: String): Option[A] = atomic.fromString(s)
  override def stringRepr(a: A): String = atomic.stringRepr(a)
}

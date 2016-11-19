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

import java.util.UUID

import cats.Eval
import cats.data.{ State, EitherT }
import cats.implicits._

import shapeless._

private sealed trait ModelRepr {

  import ModelRepr._

  final def toModel(reg: AtomRegistry): Either[String, Model] = {
    val res = toModelSt(reg).value.runA(Map.empty).value
    // do a full traverse, to allow
    // dropping thunks and to ensure
    // that any bug in the decoding
    // will cause an early failure:
    res.map { mod =>
      val _ = mod.desc
      mod
    }
  }

  protected def toModelSt(reg: AtomRegistry): DecSt[Model]
}

private object ModelRepr extends ModelReprBase {

  private type Error = String
  private type DecSt[A] = EitherT[State[DecMap, ?], Error, A]
  private type DecMap = Map[Int, Model]

  // TODO: try to simplify
  sealed trait Composite[C <: Model, T <: Model, TR <: ModelRepr] { this: ModelRepr =>

    def id: Int
    def head: ModelRepr
    def tail: TR

    protected def desc: String

    protected def build(h: Eval[Model], t: Eval[T]): C

    protected def decTail(tr: TR, reg: AtomRegistry): DecSt[T]

    protected def toCompositeModel(reg: AtomRegistry): DecSt[C] = {
      val x = for {
        st <- State.get[DecMap]
      } yield {
        lazy val res: C = build(
          Eval.later(h.value._2.getOrElse(impossible(s"accessing ${desc} parent of invalid `h`"))),
          Eval.later(t.value._2.getOrElse(impossible(s"accessing ${desc} parent of invalid `t`")))
        )
        lazy val newSt: DecMap = st + (id -> res)
        lazy val h = head.toModelSt(reg).value.run(newSt)
        lazy val lh = Eval.later(h.value._2)
        lazy val t = decTail(tail, reg).value.run(h.value._1)
        lazy val lt = Eval.later(t.value._2)
        (res, t.value._1, (lh, lt))
      }

      EitherT {
        x.flatMap {
          case (c, map, (h, t)) =>
            // now check whether decoding
            // `h` and `t` succeeds:
            val res = for {
              _ <- h.value
              _ <- t.value
            } yield c

            State.set(map).flatMap { _ =>
              res.fold(
                failure => {
                  // either `h` or `t` failed, so we
                  // must remove the (invalid) inserted
                  // composite instance:
                  State.modify[DecMap](_ - id).map(_ => res)
                },
                _ => State.pure(res)
              )
            }
        }
      }
    }
  }

  sealed trait ProdRepr extends ModelRepr {

    protected final override def toModelSt(reg: AtomRegistry): DecSt[Model] =
      toProdSt(reg).map[Model](identity)

    private[ModelRepr] def toProdSt(reg: AtomRegistry): DecSt[Model.HList]
  }

  final case object HNil extends ProdRepr {

    private[ModelRepr] override def toProdSt(reg: AtomRegistry): DecSt[Model.HList] =
      EitherT.liftT(State.pure(Model.HNil))
  }

  final case class HCons(
    id: Int,
    label: Symbol,
    optional: Boolean = false,
    head: ModelRepr,
    tail: ProdRepr
  ) extends ProdRepr with Composite[Model.HCons, Model.HList, ProdRepr] {

    protected override val desc = "HCons"

    protected override def build(h: Eval[Model], t: Eval[Model.HList]): Model.HCons =
      Model.HCons(label, optional, h.value, t.value)

    protected override def decTail(tr: ProdRepr, reg: AtomRegistry): DecSt[Model.HList] =
      tr.toProdSt(reg)

    private[ModelRepr] override def toProdSt(reg: AtomRegistry): DecSt[Model.HList] =
      toCompositeModel(reg).map(identity)
  }

  sealed trait SumRepr extends ModelRepr {

    protected final override def toModelSt(reg: AtomRegistry): DecSt[Model] =
      toSumSt(reg).map[Model](identity)

    private[ModelRepr] def toSumSt(reg: AtomRegistry): DecSt[Model.Coproduct]
  }

  final case object CNil extends SumRepr {

    private[ModelRepr] override def toSumSt(reg: AtomRegistry): DecSt[Model.Coproduct] =
      EitherT.liftT(State.pure(Model.CNil))
  }

  final case class CCons(
    id: Int,
    label: Symbol,
    head: ModelRepr,
    tail: SumRepr
  ) extends SumRepr with Composite[Model.CCons, Model.Coproduct, SumRepr] {

    protected override val desc = "CCons"

    protected override def build(h: Eval[Model], t: Eval[Model.Coproduct]): Model.CCons =
      Model.CCons(label, h.value, t.value)

    protected override def decTail(tr: SumRepr, reg: AtomRegistry): DecSt[Model.Coproduct] =
      tr.toSumSt(reg)

    private[ModelRepr] override def toSumSt(reg: AtomRegistry): DecSt[Model.Coproduct] =
      toCompositeModel(reg).map(identity)
  }

  final case class Vector(elems: ModelRepr) extends ModelRepr {
    protected override def toModelSt(reg: AtomRegistry): DecSt[Model] =
      elems.toModelSt(reg).map(e => Model.Vector(e))
  }

  final case class Atom(id: UUID) extends ModelRepr {
    protected override def toModelSt(reg: AtomRegistry): DecSt[Model] =
      EitherT.fromEither(reg.getAtom(id))
  }

  final case class Ref(id: Int) extends ModelRepr {
    protected override def toModelSt(reg: AtomRegistry): DecSt[Model] = {
      EitherT(State.get[DecMap].map { map =>
        Either.fromOption(map.get(id), s"invalid ID: $id")
      })
    }
  }

  def fromModel(model: Model): ModelRepr = {
    val map: Map[Model, Int] = model.localIds
    model.foldC[ModelRepr](
      hNil = _ => HNil,
      hCons = { (c, l, o, h, t) =>
        val id = map.get(c.m).getOrElse {
          impossible(s"no ID found for HCons at ${c.p} (map is ${map})")
        }
        t match {
          case t: ProdRepr => HCons(id, l, o, h, t)
          case _ => impossible("tail of HCons is not a product")
        }
      },
      cNil = _ => CNil,
      cCons = { (c, l, h, t) =>
        val id = map.get(c.m).getOrElse {
          impossible(s"no ID found for CCons at ${c.p} (map is ${map})")
        }
        t match {
          case t: SumRepr => CCons(id, l, h, t)
          case _ => impossible("tail of CCons is not a sum")
        }
      },
      vector = (c, e) => Vector(e),
      atom = (_, a) => Atom(a.uuid),
      cycle = { c =>
        val id = map.get(c.m).getOrElse {
          impossible(s"no ID found for cycle at ${c.p} (map is ${map})")
        }
        Ref(id)
      }
    )
  }
}

/**
 * This is a workaround for SI-7046.
 */
private sealed trait ModelReprBase {

  implicit val labelledGenericForModelRepr =
    cachedImplicit[LabelledGeneric[ModelRepr]]

  implicit val reifiedForModelRepr: Reified.Aux[ModelRepr, Model.CCons, Reified.FFirst] =
    cachedImplicit[Reified[ModelRepr]]
}

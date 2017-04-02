/*
 * Copyright 2016-2017 Daniel Urban
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

  final def toModel: Either[String, Model] = {
    val res = toModelSt.value.runA(Map.empty).value
    // do a full traverse, to allow
    // dropping thunks and to ensure
    // that any bug in the decoding
    // will cause an early failure:
    res.map { mod =>
      val _ = mod.desc
      mod
    }
  }

  protected def toModelSt: DecSt[Model]
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

    protected def decTail(tr: TR): DecSt[T]

    protected def toCompositeModel: DecSt[C] = {
      val x = for {
        st <- State.get[DecMap]
      } yield {
        lazy val res: C = build(
          Eval.later(h.value._2.getOrElse(impossible(sh"accessing ${desc} parent of invalid `h`"))),
          Eval.later(t.value._2.getOrElse(impossible(sh"accessing ${desc} parent of invalid `t`")))
        )
        lazy val newSt: DecMap = st + (id -> res)
        lazy val h = head.toModelSt.value.run(newSt)
        lazy val lh = Eval.later(h.value._2)
        lazy val t = decTail(tail).value.run(h.value._1)
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

    protected final override def toModelSt: DecSt[Model] =
      toProdSt.map[Model](identity)

    private[ModelRepr] def toProdSt: DecSt[Model.HList]
  }

  final case object HNil extends ProdRepr {

    private[ModelRepr] override def toProdSt: DecSt[Model.HList] =
      EitherT.liftT(State.pure(Model.HNil))
  }

  final case class HCons(
    id: Int,
    label: Symbol,
    optional: Boolean = false,
    refinement: Option[Model.Ref] = None,
    head: ModelRepr,
    tail: ProdRepr
  ) extends ProdRepr with Composite[Model.HCons[_], Model.HList, ProdRepr] {

    protected override val desc = "HCons"

    protected override def build(h: Eval[Model], t: Eval[Model.HList]): Model.HCons[_] =
      Model.HCons(label, optional, refinement, h.value, t.value)

    protected override def decTail(tr: ProdRepr): DecSt[Model.HList] =
      tr.toProdSt

    private[ModelRepr] override def toProdSt: DecSt[Model.HList] =
      toCompositeModel.map(identity)
  }

  sealed trait SumRepr extends ModelRepr {

    protected final override def toModelSt: DecSt[Model] =
      toSumSt.map[Model](identity)

    private[ModelRepr] def toSumSt: DecSt[Model.Coproduct]
  }

  final case object CNil extends SumRepr {

    private[ModelRepr] override def toSumSt: DecSt[Model.Coproduct] =
      EitherT.liftT(State.pure(Model.CNil))
  }

  final case class CCons(
    id: Int,
    label: Symbol,
    refinement: Option[Model.Ref] = None,
    head: ModelRepr,
    tail: SumRepr
  ) extends SumRepr with Composite[Model.CCons, Model.Coproduct, SumRepr] {

    protected override val desc = "CCons"

    protected override def build(h: Eval[Model], t: Eval[Model.Coproduct]): Model.CCons =
      Model.CCons(label, refinement, h.value, t.value)

    protected override def decTail(tr: SumRepr): DecSt[Model.Coproduct] =
      tr.toSumSt

    private[ModelRepr] override def toSumSt: DecSt[Model.Coproduct] =
      toCompositeModel.map(identity)
  }

  final case class Vector(refinement: Option[Model.Ref] = None, elems: ModelRepr) extends ModelRepr {
    protected override def toModelSt: DecSt[Model] =
      elems.toModelSt.map(els => Model.Vector(els, refinement))
  }

  final case class Atom(id: UUID, desc: String) extends ModelRepr {
    protected override def toModelSt: DecSt[Model] =
      EitherT.fromEither(Right(Model.Atom(id, desc)))
  }

  final case class Ref(id: Int) extends ModelRepr {
    protected override def toModelSt: DecSt[Model] = {
      EitherT(State.get[DecMap].map { map =>
        Either.fromOption(map.get(id), sh"invalid ID: $id")
      })
    }
  }

  def fromModel(model: Model): ModelRepr = {
    val map: Map[Model, Int] = model.localIds
    model.foldC[ModelRepr](
      hNil = _ => HNil,
      hCons = { (c, l, o, r, h, t) =>
        val id = map.get(c.m).getOrElse {
          impossible(sh"no ID found for HCons at ${c.p} (map is ${map})")
        }
        t match {
          case t: ProdRepr => HCons(id, l, o, r, h, t)
          case _ => impossible("tail of HCons is not a product")
        }
      },
      cNil = _ => CNil,
      cCons = { (c, l, r, h, t) =>
        val id = map.get(c.m).getOrElse {
          impossible(sh"no ID found for CCons at ${c.p} (map is ${map})")
        }
        t match {
          case t: SumRepr => CCons(id, l, r, h, t)
          case _ => impossible("tail of CCons is not a sum")
        }
      },
      vector = (_, r, e) => Vector(r, e),
      atom = (_, a) => Atom(a.uuid, a.atomDesc),
      cycle = { c =>
        val id = map.get(c.m).getOrElse {
          impossible(sh"no ID found for cycle at ${c.p} (map is ${map})")
        }
        Ref(id)
      }
    )
  }

  val modelRefinement: Refinement.Aux[Model, ModelRepr] = new Refinement[Model] {

    override type Repr = ModelRepr

    override val uuid =
      uuid"61989ad2-2423-4018-98ab-5309bc863e3c"

    override val repr =
      Refinement.ReprFormat.single("ModelRepr")

    override def from(r: ModelRepr) =
      r.toModel

    override def to(m: Model): ModelRepr =
      fromModel(m)
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

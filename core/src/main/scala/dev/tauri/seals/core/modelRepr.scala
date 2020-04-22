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

import cats.{ Eval, Eq }
import cats.implicits._

import shapeless._

private sealed trait ModelRepr {

  import ModelDecoding._
  import ModelRepr._

  final def toModel: Either[String, Model] = {
    Api.procInstance[DecMap].force(toModelSt[DecSt](Api.procInstance), HMap.empty).flatMap { mod =>
      try {
        // We do a full traverse, to allow
        // dropping thunks and to ensure
        // that any bug in the decoding
        // will cause an early failure.
        // Also, reference errors will be
        // thrown here as `DecodingError`s.
        mod.desc
        // OK, no bugs or reference errors:
        Right(mod)
      } catch {
        case DecodingError(err) =>
          Left(err)
      }
    }
  }

  protected def toModelSt[F[_] : DecCtx]: F[Model]
}

private object ModelRepr extends ModelReprBase {

  private type DecSt[A] = ModelDecoding.Api.F[DecMap, A]
  private type DecCtx[F[_]] = ModelDecoding.Proc[F, DecMap]

  type DecMap = HMap[DecMapRel]
  final class DecMapRel[K, V]
  final object DecMapRel {
    implicit def ref[M <: Model]: DecMapRel[Ref[M], M] = new DecMapRel
  }

  // TODO: try to simplify
  sealed trait Composite[C <: T, T <: Model, TR <: ModelRepr] { this: ModelRepr =>

    def ref: Ref[T]
    def head: ModelRepr
    def tail: TR

    protected def desc: String

    protected def build(h: Eval[Model], t: Eval[T]): C

    protected def decTail[F[_] : DecCtx](tr: TR): F[T]

    protected def toCompositeModel[F[_]](implicit F: DecCtx[F]): F[C] = {
      F.flatMap(F.map(F.get) { st =>
        // Note: reference errors will cause
        // these exceptions to be thrown. That
        // is why in `toModel` we catch these
        // exceptions, and convert them into
        // proper errors (see there).
        // (Unfortunately, here we're relying
        // on the fact that Proc.raise is the
        // same as throwing a DecodingError.)
        lazy val res: C = build(
          Eval.later(h._2.fold(e => throw new ModelDecoding.DecodingError(e), m => m)),
          Eval.later(t._2.fold(e => throw new ModelDecoding.DecodingError(e), m => m))
        )
        lazy val newSt: DecMap = st + (ref -> (res : T))
        lazy val h = F.forceAS(head.toModelSt[F], newSt)
        lazy val t = F.forceAS(decTail[F](tail), h._1)
        (res, t._1)
      }) { case (c, map) =>
        // Note: we don't force `h` and `t`
        // right now, because that would cause
        // possibly exponential runtime for
        // decoding. We force everything all
        // at once at the end (see `toModel`).
        F.map(F.set(map))(_ => c)
      }
    }
  }

  sealed trait ProdRepr extends ModelRepr {

    protected final override def toModelSt[F[_]](implicit F: DecCtx[F]): F[Model] = {
      F.map[Model.HList, Model](toProdSt[F])(identity)
    }

    private[ModelRepr] def toProdSt[F[_] : DecCtx]: F[Model.HList]
  }

  final case object HNil extends ProdRepr {

    private[ModelRepr] override def toProdSt[F[_]](implicit F: DecCtx[F]): F[Model.HList] =
      F.pure(Model.HNil)
  }

  final case class HCons(
    id: Int,
    label: Symbol,
    optional: Boolean = false,
    refinement: Option[Model.Ref] = None,
    head: ModelRepr,
    tail: ProdRepr
  ) extends ProdRepr with Composite[Model.HCons[_], Model.HList, ProdRepr] {

    override val ref: Ref[Model.HList] = ProdRef(id)

    protected override val desc = "HCons"

    protected override def build(h: Eval[Model], t: Eval[Model.HList]): Model.HCons[_] =
      Model.HCons(label, optional, refinement, h.value, t.value)

    protected override def decTail[F[_]](tr: ProdRepr)(implicit F: DecCtx[F]): F[Model.HList] =
      tr.toProdSt[F]

    private[ModelRepr] override def toProdSt[F[_]](implicit F: DecCtx[F]): F[Model.HList] = {
      F.map(toCompositeModel[F])(identity)
    }
  }

  sealed trait SumRepr extends ModelRepr {

    protected final override def toModelSt[F[_]](implicit F: DecCtx[F]): F[Model] =
      F.map(toSumSt[F])(identity)

    private[ModelRepr] def toSumSt[F[_]](implicit F: DecCtx[F]): F[Model.Coproduct]
  }

  final case object CNil extends SumRepr {

    private[ModelRepr] override def toSumSt[F[_]](implicit F: DecCtx[F]): F[Model.Coproduct] =
      F.pure(Model.CNil)
  }

  final case class CCons(
    id: Int,
    label: Symbol,
    refinement: Option[Model.Ref] = None,
    head: ModelRepr,
    tail: SumRepr
  ) extends SumRepr with Composite[Model.CCons, Model.Coproduct, SumRepr] {

    override val ref: Ref[Model.Coproduct] = SumRef(id)

    protected override val desc = "CCons"

    protected override def build(h: Eval[Model], t: Eval[Model.Coproduct]): Model.CCons =
      Model.CCons(label, refinement, h.value, t.value)

    protected override def decTail[F[_]](tr: SumRepr)(implicit F: DecCtx[F]): F[Model.Coproduct] =
      tr.toSumSt[F]

    private[ModelRepr] override def toSumSt[F[_]](implicit F: DecCtx[F]): F[Model.Coproduct] = {
      F.map(toCompositeModel[F])(identity)
    }
  }

  final case class Vector(refinement: Option[Model.Ref] = None, elems: ModelRepr) extends ModelRepr {
    protected override def toModelSt[F[_]](implicit F: DecCtx[F]): F[Model] = {
      F.map(elems.toModelSt[F])(els => Model.Vector(els, refinement))
    }
  }

  final case class Atom(id: UUID, desc: String) extends ModelRepr {
    protected override def toModelSt[F[_]](implicit F: DecCtx[F]): F[Model] =
      F.pure(Model.Atom(id, desc))
  }

  // TODO: maybe optimize encoding, to on the wire all refs are a simple `Ref(id)`
  sealed abstract class Ref[M <: Model](val id: Int) extends ModelRepr {
    protected def toModelStImpl[F[_]](implicit F: DecCtx[F]): F[M] = {
      F.flatMap(F.get) { map =>
        map.get(this) match {
          case Some(r) => F.pure(r)
          case None => F.raise(sh"invalid ID: $id")
        }
      }
    }
  }

  final case class ProdRef(override val id: Int) extends Ref[Model.HList](id) with ProdRepr {
    override def toProdSt[F[_]](implicit F: DecCtx[F]): F[Model.HList] = toModelStImpl[F]
  }

  final case class SumRef(override val id: Int) extends Ref[Model.Coproduct](id) with SumRepr {
    override def toSumSt[F[_]](implicit F: DecCtx[F]): F[Model.Coproduct] = toModelStImpl[F]
  }

  final case class OtherRef(override val id: Int) extends Ref[Model](id) {
    protected final override def toModelSt[F[_]](implicit F: DecCtx[F]) = toModelStImpl[F]
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
        c.m match {
          case _: Model.CCons | Model.CNil => SumRef(id)
          case _: Model.HCons[_] | Model.HNil => ProdRef(id)
          case _ => OtherRef(id)
        }
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

  implicit val eqInstance: Eq[ModelRepr] =
    Eq.fromUniversalEquals[ModelRepr] // only case classes/objects
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

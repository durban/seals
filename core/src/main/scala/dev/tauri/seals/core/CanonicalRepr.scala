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

import cats.Order
import cats.implicits._

sealed trait CanonicalRepr extends Serializable

object CanonicalRepr {

  def fold[A](a: A)(implicit r: Reified[A]): CanonicalRepr = {
    r.close[CanonicalRepr, CanonicalRepr](
      r.fold[CanonicalRepr, CanonicalRepr](a)(folder),
      identity
    )
  }

  def unfold[A](repr: CanonicalRepr)(implicit r: Reified[A]): Either[String, A] =
    r.unfold(unfolder)(repr).map(_._1)

  def roundtrip[A](a: A)(implicit r: Reified[A]): A = {
    unfold(fold(a)(r))(r).fold(
      err => core.impossible(sh"cannot unfold folded CanonicalRepr: ${err}"),
      a => a
    )
  }

  def product(els: (Symbol, CanonicalRepr)*): CanonicalRepr = {
    els.foldRight[CanonicalRepr](CanonicalRepr.HNil) { case ((lab, r), acc) => CanonicalRepr.HCons(lab, r, acc) }
  }

  implicit val orderForCanonicalRepr: Order[CanonicalRepr] = {
    case (Atom(x), Atom(y)) =>
      Order[String].compare(x, y)
    case (Atom(_), _) =>
      -1
    case (HNil, Atom(_)) =>
      1
    case (HNil, HNil) =>
      0
    case (HNil, _) =>
      -1
    case (HCons(_, _, _), Atom(_) | HNil) =>
      1
    case (HCons(xl, xh, xt), HCons(yl, yh, yt)) =>
      cats.instances.tuple.catsKernelStdOrderForTuple3(
        Order[Symbol],
        orderForCanonicalRepr,
        orderForCanonicalRepr
      ).compare((xl, xh, xt), (yl, yh, yt))
    case (HCons(_, _, _), _) =>
      -1
    case (Sum(_, _), Atom(_) | HNil | HCons(_, _, _)) =>
      1
    case (Sum(xl, xv), Sum(yl, yv)) =>
      if (xl === yl) orderForCanonicalRepr.compare(xv, yv) else Order[Symbol].compare(xl, yl)
    case (Sum(_, _), _) =>
      -1
    case (Vect(_), Atom(_) | HNil | HCons(_, _, _) | Sum(_, _)) =>
      1
    case (Vect(x), Vect(y)) =>
      cats.instances.vector.catsKernelStdOrderForVector(orderForCanonicalRepr).compare(x, y)
  }

  val folder: Reified.Folder[CanonicalRepr, CanonicalRepr] = Reified.Folder.simple(
    atom = a => Atom(a.stringRepr),
    hNil = () => HNil,
    hCons = HCons.apply,
    sum = Sum.apply,
    vector = Vect.apply
  )

  val unfolder: Reified.Unfolder[CanonicalRepr, String, Vector[CanonicalRepr]] = Reified.Unfolder.instance(
    atom = {
      case a @ Atom(r) => Right(Reified.StringResult(r, a))
      case _ => Left("not an atom")
    },
    atomErr = (_, err) => sh"cannot decode atom: '${err.msg}'",
    hNil = {
      case hn: HNil.type => Right(hn)
      case hc @ HCons(_, _, _) => Right(hc) // ignore unneeded field
      case _ => Left("not HNil (or ignored HCons)")
    },
    hCons = {
      case (HCons(l, h, t), sym) if l === sym => Right(Right((h, _ => Right(t))))
      case _ => Left("not HCons")
    },
    cNil = _ => "CNil reached",
    cCons = {
      case (s @ Sum(l, v), sym) => if (l === sym) {
        Right(Left(v))
      } else {
        Right(Right(s))
      }
      case _ => Left("not CCons")
    },
    vectorInit = {
      case v @ Vect(els) => Right((v, els))
      case _ => Left("not Vector")
    },
    vectorFold = {
      case (_, h +: t) => Right(Some((h, t)))
      case (_, Vector()) => Right(None)
      case _ => Left("not Vector")
    },
    unknownError = identity
  )

  final case class Atom(repr: String) extends CanonicalRepr
  final case object HNil extends CanonicalRepr
  final case class HCons(label: Symbol, head: CanonicalRepr, tail: CanonicalRepr)
    extends CanonicalRepr
  final case class Sum(label: Symbol, value: CanonicalRepr)
    extends CanonicalRepr
  final case class Vect(elems: Vector[CanonicalRepr])
    extends CanonicalRepr
}

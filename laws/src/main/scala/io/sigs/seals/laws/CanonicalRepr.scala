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
package laws

import cats.Eq
import cats.implicits._

import core.symbolEq

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

  implicit val eqForCanonicalRepr: Eq[CanonicalRepr] =
    Eq.fromUniversalEquals

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
    atomErr = (c, err) => sh"cannot decode atom: '${err.msg}'",
    hNil = {
      case hn @ HNil => Right(hn)
      case hc @ HCons(_, _, _) => Right(hc) // ignore unneeded field
      case _ => Left("not HNil (or ignored HCons)")
    },
    hCons = {
      case (hc @ HCons(l, h, t), sym) if l === sym => Right(Right((h, _ => Right(t))))
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

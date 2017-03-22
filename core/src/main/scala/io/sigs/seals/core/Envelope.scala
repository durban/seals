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

import scala.util.hashing.MurmurHash3

import cats.{ Eq, Show }
import cats.implicits._

sealed trait Envelope[A] extends Serializable {

  def value: A

  def reified: Reified[A]

  final def model: Model =
    reified.model

  final override def equals(that: Any): Boolean = that match {
    case that: Envelope[_] =>
      this.value == that.value
    case _ =>
      false
  }

  final override def hashCode: Int = {
    val s = MurmurHash3.mixLast(Envelope.hashSeed, value.##)
    MurmurHash3.finalizeHash(s, 1)
  }

  final override def toString: String =
    show(Show.fromToString[A])

  final def show(implicit A: Show[A]): String =
    sh"Envelope[${model}](${value})"
}

object Envelope {

  private final case class EnvelopeRepr[A](model: Model, value: A)

  private[seals] final val hashSeed = 0x37dd86e4

  def apply[A](a: A)(implicit r: Reified[A]): Envelope[A] = new Envelope[A] {
    override val value = a
    override val reified = r
  }

  implicit def envelopeEquality[A](implicit EqA: Eq[A]): Eq[Envelope[A]] = new Eq[Envelope[A]] {
    override def eqv(x: Envelope[A], y: Envelope[A]): Boolean =
      EqA.eqv(x.value, y.value)
  }

  implicit def envelopeShow[A](implicit A: Show[A]): Show[Envelope[A]] =
    Show.show(env => env.show(A))

  private def refinement[A](implicit r: Reified[A]): Refinement.Aux[Envelope[A], EnvelopeRepr[A]] = {
    new Refinement[Envelope[A]] {
      override type Repr = EnvelopeRepr[A]
      // TODO:
      override val uuid = null // scalastyle:ignore null
      override def repr = Refinement.ReprFormat.single("✉")
      override def from(repr: Repr) = {
        if (repr.model compatible r.model) Either.right(Envelope[A](repr.value)(r))
        else Either.left(sh"incompatible models: expected '${r.model}', got '${repr.model}'")
      }
      override def to(env: Envelope[A]) = {
        EnvelopeRepr[A](env.model, env.value)
      }
    }
  }

  // TODO: Reified[EnvelopeRepr[A]].refined(refinement)(null : Model.CanBeRefined[Model.HList])
  implicit def reifiedForEnvelope[A](implicit r: Reified[A]): Reified[Envelope[A]] = {
    Reified[EnvelopeRepr[A]].pimapOld[Envelope[A]] { repr =>
      if (repr.model compatible r.model) Either.right(Envelope[A](repr.value)(r))
      else Either.left(sh"incompatible models: expected '${r.model}', got '${repr.model}'")
    } { env =>
      EnvelopeRepr[A](env.model, env.value)
    }
  }
}

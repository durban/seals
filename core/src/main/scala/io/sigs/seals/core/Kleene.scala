/*
 * Copyright 2016 Daniel Urban and contributors listed in AUTHORS
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

import cats.~>
import shapeless.poly

trait Kleene[F[_]] extends Serializable {

  def toVector[A](fa: F[A]): Vector[A]

  def fromVector[A](v: Vector[A]): F[A]
}

object Kleene {

  def apply[F[_]](implicit inst: Kleene[F]): Kleene[F] =
    inst

  def instance[F[_]](toVect: F ~> Vector, fromVect: Vector ~> F): Kleene[F] = {
    new Kleene[F] {
      override def toVector[A](fa: F[A]): Vector[A] =
        toVect(fa)
      override def fromVector[A](v: Vector[A]): F[A] =
        fromVect(v)
    }
  }

  def instance[F[_]](toVect: poly.~>[F, Vector], fromVect: poly.~>[Vector, F]): Kleene[F] = {
    new Kleene[F] {
      override def toVector[A](fa: F[A]): Vector[A] =
        toVect(fa)
      override def fromVector[A](v: Vector[A]): F[A] =
        fromVect(v)
    }
  }

  implicit val kleeneForVector: Kleene[Vector] = Kleene.instance(
    λ[Vector ~> Vector](x => x),
    λ[Vector ~> Vector](x => x)
  )

  implicit val kleeneForList: Kleene[List] = Kleene.instance(
    λ[List ~> Vector](_.toVector),
    λ[Vector ~> List](_.toList)
  )
}

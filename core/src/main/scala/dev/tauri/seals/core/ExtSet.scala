/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.implicits._

/** Type class for (extensional) set-like data stuctures */
trait ExtSet[F[_], A] extends Serializable {

  /** Unique, but not necessarily sorted elements */
  def toVector(fa: F[A]): Vector[A]

  /** Must reject if there are duplicate elements */
  def fromVector(v: Vector[A]): Either[String, F[A]]
}

final object ExtSet {

  def apply[F[_], A](implicit inst: ExtSet[F, A]): ExtSet[F, A] =
    inst

  def instance[F[_], A](toVect: F[A] => Vector[A], fromVect: Vector[A] => Either[String, F[A]]): ExtSet[F, A] = {
    new ExtSet[F, A] {
      override def toVector(fa: F[A]): Vector[A] =
        toVect(fa)
      override def fromVector(v: Vector[A]): Either[String, F[A]] =
        fromVect(v)
    }
  }

  implicit def setLikeForSet[A]: ExtSet[Set, A] = instance(
    _.toVector,
    { vec =>
      val set = vec.toSet
      if (set.size === vec.size) Right(set)
      else Left("duplicate elements")
    }
  )
}

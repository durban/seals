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

// TODO: laws

trait Kleene[F[_]] extends Serializable {

  def toVector[A](fa: F[A]): Vector[A]

  def fromVector[A](l: Vector[A]): F[A]
}

object Kleene {

  implicit val kleeneForVector: Kleene[Vector] = new Kleene[Vector] {
    override def toVector[A](fa: Vector[A]): Vector[A] =
      fa
    override def fromVector[A](v: Vector[A]): Vector[A] =
      v
  }

  implicit val kleeneForList: Kleene[List] = new Kleene[List] {
    override def toVector[A](fa: List[A]): Vector[A] =
      fa.toVector
    override def fromVector[A](v: Vector[A]): List[A] =
      v.toList
  }
}

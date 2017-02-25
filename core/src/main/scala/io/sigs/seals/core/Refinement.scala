/*
 * Copyright 2017 Daniel Urban
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

import cats.kernel.Order
import cats.implicits._

trait Refinement[A] extends Serializable {

  type Repr

  def uuid: UUID

  def desc(r: String): String = s"${r}{?}"

  def from(a: Repr): Either[String, A]

  def to(a: A): Repr

  /** Convenience for implementations */
  protected def root: UUID =
    Refinement.root

  final override def equals(that: Any): Boolean = that match {
    case that: Refinement[_] =>
      this.uuid === that.uuid
    case _ =>
      false
  }

  final override def hashCode: Int =
    this.uuid.##
}

object Refinement {

  type Aux[A, R] = Refinement[A] {
    type Repr = R
  }

  final val root = uuid"cc154e4c-24b8-4505-a13c-59b548ec9883"

  final val ge = uuid"ff6383db-8d2e-4507-a571-f6f0f73f1fe8"
  final val le = uuid"95d56687-589e-4e8a-8857-0707ad3cd60b"
  final val en = uuid"5c7fe757-72c0-4114-9ed0-06e8a8d34c04"

  def enum[A](implicit A: EnumLike[A]): Refinement.Aux[A, Int] = new Refinement[A] {
    override type Repr = Int
    override val uuid = (root / en / Atomic[Int].binaryRepr(A.maxIndex)).uuid
    override def desc(r: String) = s"0 ≤ ${r} ≤ ${A.maxIndex}"
    def from(idx: Int) = A.fromIndex(idx)
    def to(a: A) = A.index(a)
  }

  def greaterEqual[A](than: A)(implicit A: Order[A], atc: Atomic[A]): Aux[A, A] = new Refinement[A] {
    override type Repr = A
    override val uuid = (root / ge / atc.binaryRepr(than)).uuid
    override def desc(r: String) = s"${r} ≥ ${than}"
    def from(a: A) = if (A.gteqv(a, than)) Right(a) else Left(s"${a} ≱ ${than}")
    def to(a: A) = a
  }

  def lessEqual[A](than: A)(implicit A: Order[A], atc: Atomic[A]): Aux[A, A] = new Refinement[A] {
    override type Repr = A
    override val uuid = (root / le / atc.binaryRepr(than)).uuid
    override def desc(r: String) = s"${r} ≤ ${than}"
    def from(a: A) = if (A.lteqv(a, than)) Right(a) else Left(s"${a} ≰ ${than}")
    def to(a: A) = a
  }
}

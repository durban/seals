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

import scala.util.hashing.MurmurHash3
import cats.Eq

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
    val s = MurmurHash3.mixLast(Envelope.envelopeSeed, value.##)
    MurmurHash3.finalizeHash(s, 1)
  }
}

object Envelope {

  private final val envelopeSeed = 0x37dd86e4

  def apply[A](a: A)(implicit r: Reified[A]): Envelope[A] = new Envelope[A] {
    override val value = a
    override val reified = r
  }

  implicit def envelopeEquality[A](implicit EqA: Eq[A]): Eq[Envelope[A]] = new Eq[Envelope[A]] {
    override def eqv(x: Envelope[A], y: Envelope[A]): Boolean =
      EqA.eqv(x.value, y.value)
  }
}

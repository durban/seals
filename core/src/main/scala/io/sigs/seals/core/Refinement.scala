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

import cats.Show
import cats.implicits._

import _root_.scodec.bits.ByteVector

trait Refinement[A] extends Serializable {

  import Refinement._

  type Repr

  def uuid: UUID

  def repr: ReprFormat =
    ReprFormat("", true, "{?}")

  final def semantics: Semantics =
    Semantics(uuid, repr)

  final def desc(r: String): String =
    repr.repr(r)

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

  override def toString: String =
    desc("?")
}

object Refinement {

  type Aux[A, R] = Refinement[A] {
    type Repr = R
  }

  final case class ReprFormat(pre: String, mid: Boolean, post: String) {

    def repr(r: String): String = {
      if (mid) pre + r + post
      else pre + post
    }

    def combine(that: ReprFormat): ReprFormat = {
      if (that.mid) {
        ReprFormat(that.pre + this.pre, this.mid, this.post + that.post)
      } else {
        that
      }
    }
  }

  object ReprFormat {
    def single(s: String): ReprFormat =
      ReprFormat(s, false, "")
  }

  final case class Semantics(uuid: UUID, repr: ReprFormat) {

    def desc(r: String): String =
      repr.repr(r)

    final override def equals(that: Any): Boolean = that match {
      case Semantics(thatId, _) =>
        uuid === thatId
      case _ =>
        false
    }

    final override def hashCode: Int =
      uuid.##
  }

  object Semantics {

    import java.io.ByteArrayOutputStream
    import java.nio.charset.StandardCharsets.UTF_8

    private[this] final class FoldBytes extends Reified.Folder[ByteArrayOutputStream, ByteArrayOutputStream] {

      private[this] val buf = new ByteArrayOutputStream

      def atom(repr: Reified.AtomRepr): ByteArrayOutputStream = {
        buf.write(repr.binaryRepr.toArray)
        buf
      }

      def hCons(l: Symbol, b: ByteArrayOutputStream, t: ByteArrayOutputStream): ByteArrayOutputStream = {
        assert(b eq t)
        b.write(0x01)
        b.write(l.name.getBytes(UTF_8))
        b
      }

      def hNil: ByteArrayOutputStream = {
        buf.write(0x00)
        buf
      }

      def prod(t: ByteArrayOutputStream): ByteArrayOutputStream = {
        assert(t eq buf)
        t
      }

      def sum(l: Symbol, b: ByteArrayOutputStream): ByteArrayOutputStream = {
        assert(b eq buf)
        b.write(0x02)
        b.write(l.name.getBytes(UTF_8))
        b
      }

      def vector(v: Vector[ByteArrayOutputStream]): ByteArrayOutputStream = {
        assert(v.forall(_ eq buf))
        buf.write(0x03)
        buf
      }
    }

    def greater[A: Show](than: A)(implicit A: Reified[A]): Semantics = {
      val repr = ByteVector.view(A.foldClose(than)(new FoldBytes).toByteArray())
      Semantics((root / gt / repr).uuid, ReprFormat("", true, sh" > ${than}"))
    }

    def less[A: Show](than: A)(implicit A: Reified[A]): Semantics = {
      val repr = ByteVector.view(A.foldClose(than)(new FoldBytes).toByteArray())
      Semantics((root / lt / repr).uuid, ReprFormat("", true, sh" < ${than}"))
    }
  }

  final val root = uuid"cc154e4c-24b8-4505-a13c-59b548ec9883"

  private[this] final val gt = uuid"ff6383db-8d2e-4507-a571-f6f0f73f1fe8"
  private[this] final val lt = uuid"95d56687-589e-4e8a-8857-0707ad3cd60b"
  private[this] final val en = uuid"5c7fe757-72c0-4114-9ed0-06e8a8d34c04"

  def enum[A](implicit A: EnumLike[A]): Refinement.Aux[A, Int] = new Refinement[A] {
    override type Repr = Int
    override val uuid = (root / en / Atomic[Int].binaryRepr(A.maxIndex)).uuid
    override def repr = Refinement.ReprFormat("0 ≤ ", true, sh" ≤ ${A.maxIndex}")
    def from(idx: Int) = A.fromIndex(idx)
    def to(a: A) = A.index(a)
  }
}

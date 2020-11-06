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

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.collection.immutable.HashMap

import cats.syntax.eq._

private sealed abstract class IdentitySet[A <: AnyRef]
  extends Function1[A, Boolean] { self =>

  def iterator: Iterator[A]

  def +(a: A): IdentitySet[A]

  def -(a: A): IdentitySet[A]

  def contains(a: A): Boolean

  def size: Int

  final def apply(a: A): Boolean =
    this contains a

  def toList: List[A] =
    this.iterator.toList

  def foldLeft[R](z: R)(f: (R, A) => R): R =
    this.iterator.foldLeft(z)(f)

  def union(that: IdentitySet[A]): IdentitySet[A] = {
    this.iterator.foldLeft(that) { _ + _ }
  }

  def intersect(that: IdentitySet[A]): IdentitySet[A] = {
    this.iterator.foldLeft(IdentitySet.empty[A]) { (set, a) =>
      if (that.contains(a)) set + a
      else set
    }
  }

  def subsetOf(that: IdentitySet[A]): Boolean = {
    this.iterator.forall(that.contains)
  }

  override def equals(that: Any): Boolean = that match {
    case that: IdentitySet[A] =>
      (this.size === that.size) && this.subsetOf(that)
    case _ =>
      false
  }
}

private object IdentitySet {

  def empty[A <: AnyRef]: IdentitySet[A] =
    identitySet0

  def of[A <: AnyRef](as: A*): IdentitySet[A] =
    as.foldLeft(empty[A]) { _ + _ }

  private def id[A <: AnyRef](a: A): Int =
    System.identityHashCode(a)

  private def lstContains[A <: AnyRef](lst: List[A], a: A): Boolean =
    lst.exists(_ eq a)

  private final object HashIdentitySet {

    def nonEmpty[A <: AnyRef](elem: A, rest: Array[A]): HashIdentitySet[A] =
      new HashIdentitySet[A](rest.foldLeft(HashMap(id(elem) -> (elem :: Nil)))(folder))

    private def folder[A <: AnyRef](map: HashMap[Int, List[A]], a: A): HashMap[Int, List[A]] = {
      val k = id(a)
      val lst = map.getOrElse(k, Nil)
      if (lstContains(lst, a)) {
        map
      } else {
        map.updated(k, a :: lst)
      }
    }
  }

  private final class HashIdentitySet[A <: AnyRef](map: HashMap[Int, List[A]])
    extends IdentitySet[A] { self =>

    def this(elems: A*) =
      this(elems.foldLeft(HashMap.empty[Int, List[A]])(HashIdentitySet.folder))

    override def -(elem: A): IdentitySet[A] = {
      val k = id(elem)
      val lst = map.getOrElse(k, Nil)
      if (lstContains(lst, elem)) {
        val newLst = lst.filter(_ ne elem)
        val m = if (newLst.nonEmpty) {
          map.updated(k, newLst)
        } else {
          map - k
        }
        new HashIdentitySet[A](m)
      } else {
        this
      }
    }

    override def +(elem: A): IdentitySet[A] = {
      val k = id(elem)
      val lst = map.getOrElse(k, Nil)
      if (lstContains(lst, elem)) {
        this
      } else {
        val m = map.updated(k, elem :: lst)
        new HashIdentitySet[A](m)
      }
    }

    override def contains(elem: A): Boolean = {
      lstContains(map.getOrElse(id(elem), Nil), elem)
    }

    override def size: Int =
      this.iterator.size

    override def iterator: Iterator[A] = new AbstractIterator[A] {

      private[this] val lsts = self.map.valuesIterator
      private[this] var curr: List[A] = Nil // scalastyle:ignore

      def hasNext: Boolean = {
        nextNonEmptyList()
        curr.nonEmpty
      }

      def next(): A = {
        nextNonEmptyList()
        curr match {
          case nxt :: rest =>
            curr = rest
            nxt
          case Nil =>
            throw new IllegalStateException
        }
      }

      @tailrec
      private def nextNonEmptyList(): Unit = {
        if (curr.isEmpty) {
          if (lsts.hasNext) {
            curr = lsts.next()
            nextNonEmptyList()
          }
        }
      }
    }
  }

  private def identitySet0[A <: AnyRef]: IdentitySet[A] =
    emptyIdentitySet.asInstanceOf[IdentitySet[A]]

  private val emptyIdentitySet: IdentitySet[AnyRef] = new IdentitySet[AnyRef] {

    final override def iterator =
      Iterator.empty

    final override def size: Int =
      0

    final override def +(a: AnyRef) =
      new IdentitySet1(a)

    final override def -(a: AnyRef) =
      this

    final override def contains(a: AnyRef): Boolean =
      false
  }

  private final class IdentitySet1[A <: AnyRef](e: A) extends IdentitySet[A] {

    final override def iterator: Iterator[A] =
      Iterator(e)

    final override def size: Int =
      1

    final override def +(a: A): IdentitySet[A] = {
      if (a eq e) this
      else new IdentitySet2(a, e)
    }

    final override def -(a: A): IdentitySet[A] = {
      if (a eq e) IdentitySet.empty
      else this
    }

    final override def contains(a: A): Boolean =
      a eq e
  }

  private final class IdentitySet2[A <: AnyRef](e1: A, e2: A) extends IdentitySet[A] {

    final override def iterator: Iterator[A] =
      Iterator(e1, e2)

    final override def size: Int =
      2

    final override def +(a: A): IdentitySet[A] = {
      if ((a eq e1) || (a eq e2)) this
      else new IdentitySet3(e1, e2, a)
    }

    final override def -(a: A): IdentitySet[A] = {
      if (a eq e1) new IdentitySet1(e2)
      else if (a eq e2) new IdentitySet1(e1)
      else this
    }

    final override def contains(a: A): Boolean =
      (a eq e1) || (a eq e2)
  }

  private final class IdentitySet3[A <: AnyRef](e1: A, e2: A, e3: A) extends IdentitySet[A] {

    final override def iterator: Iterator[A] =
      Iterator(e1, e2, e3)

    final override def size: Int =
      3

    final override def +(a: A): IdentitySet[A] = {
      if ((a eq e1) || (a eq e2) || (a eq e3)) this
      else new IdentitySetN(Array[AnyRef](e1, e2, e3, a))
    }

    final override def -(a: A): IdentitySet[A] = {
      if (a eq e1) new IdentitySet2(e2, e3)
      else if (a eq e2) new IdentitySet2(e1, e3)
      else if (a eq e3) new IdentitySet2(e1, e2)
      else this
    }

    final override def contains(a: A): Boolean =
      (a eq e1) || (a eq e2) || (a eq e3)
  }

  final val maxN = 32

  private final class IdentitySetN[A <: AnyRef](arr: Array[AnyRef]) extends IdentitySet[A] {

    final override def iterator: Iterator[A] =
      arr.iterator.asInstanceOf[Iterator[A]]

    final override def size: Int =
      arr.length

    final override def +(a: A): IdentitySet[A] = {
      if (this.contains(a)) this
      else if (arr.length < maxN) new IdentitySetN[A]({
        val newArr = Array.ofDim[AnyRef](arr.length + 1)
        System.arraycopy(arr, 0, newArr, 0, arr.length)
        newArr(arr.length) = a
        newArr
      })
      else HashIdentitySet.nonEmpty[AnyRef](a, arr).asInstanceOf[IdentitySet[A]]
    }

    final override def -(a: A): IdentitySet[A] = {
      if (this.contains(a)) {
        if (arr.length > 1) {
          val newArr = arr.filterNot(_ eq a)
          new IdentitySetN(newArr)
        } else {
          IdentitySet.empty
        }
      } else {
        this
      }
    }

    final override def contains(a: A): Boolean = {
      arr.exists(_ eq a)
    }
  }
}

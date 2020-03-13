/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.mutable.SetBuilder
import scala.collection.SetLike

private final class IdentitySet[A <: AnyRef] private (m: HashMap[Int, List[A]])
    extends Set[A] with SetLike[A, IdentitySet[A]] { self =>

  import IdentitySet._

  private[this] val map = m

  def this() = this(HashMap.empty)

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

  override def -(elem: A): IdentitySet[A] = {
    val k = id(elem)
    val lst = map.getOrElse(k, Nil)
    lst.iterator
    if (lstContains(lst, elem)) {
      val newLst = lst.filter(_ ne elem)
      val m = if (newLst.nonEmpty) {
        map.updated(k, newLst)
      } else {
        map - k
      }
      new IdentitySet[A](m)
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
      new IdentitySet[A](m)
    }
  }

  override def contains(elem: A): Boolean = {
    lstContains(map.getOrElse(id(elem), Nil), elem)
  }

  override def empty: IdentitySet[A] =
    IdentitySet.empty[A]
}

private object IdentitySet {

  def empty[A <: AnyRef]: IdentitySet[A] =
    new IdentitySet[A]

  implicit def identitySetCanBuildFrom[A <: AnyRef]: CanBuildFrom[IdentitySet[A], A, IdentitySet[A]] = {
    new CanBuildFrom[IdentitySet[A], A, IdentitySet[A]] {
      def apply(): Builder[A, IdentitySet[A]] =
        new SetBuilder[A, IdentitySet[A]](empty[A])
      def apply(from: IdentitySet[A]): Builder[A, IdentitySet[A]] =
        apply()
    }
  }

  private def id[A <: AnyRef](a: A): Int =
    System.identityHashCode(a)

  private def lstContains[A <: AnyRef](lst: List[A], a: A): Boolean =
    lst.exists(_ eq a)
}

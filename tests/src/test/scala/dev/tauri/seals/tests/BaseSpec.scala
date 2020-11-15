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
package tests

import org.scalatest.matchers.should.Matchers
import org.scalatest.freespec.AnyFreeSpecLike
import org.scalatest.Inside
import org.scalactic.TypeCheckedTripleEquals

trait BaseSpec
  extends AnyFreeSpecLike
  with Matchers
  with Inside
  with TypeCheckedTripleEquals
  with laws.Serialization {

  final val notFound =
    "could not find implicit value for parameter compat:.*"

  def checkEqHash[A](a: A, b: A): Unit = {
    a should === (b)
    b should === (a)
    a.## should === (b.##)
    a.hashCode should === (b.hashCode)
    ()
  }

  def checkEqHashCompat(a: Model, b: Model): Unit = {
    checkEqHash(a, b)
    checkCompatible(a, b)
  }

  def checkCompatible(a: Model, b: Model): Unit = {
    if (!a.compatible(b)) {
      fail(s"${a} wasn't compatible with ${b} (it should)")
    }
    if (!b.compatible(a)) {
      fail(s"${b} wasn't compatible with ${a} (it should)")
    } else {
      if (a !== b) {
        a.## should !== (b.##)
        a.hashCode should !== (b.hashCode)
        ()
      }
    }
  }

  def checkNotEqHash[A](a: A, b: A): Unit = {
    a should !== (b)
    a.## should !== (b.##)
    a.hashCode should !== (b.hashCode)
    ()
  }

  def checkNotEqHashCompat(a: Model, b: Model): Unit = {
    checkNotEqHash(a, b)
    checkNotCompatible(a, b)
  }

  def checkNotCompatible(a: Model, b: Model): Unit = {
    if (a.compatible(b)) {
      fail(s"${a} was compatible with ${b} (it shouldn't)")
    }
    if (b.compatible(a)) {
      fail(s"${b} was compatible with ${a} (it shouldn't)")
    } else {
      a.## should !== (b.##)
      a.hashCode should !== (b.hashCode)
      ()
    }
  }

  def checkSer[A <: AnyRef](a: A): A = {
    val res = roundtripSer(a)
    res should === (a)
    a should === (res)
    a.## should === (res.##)
    res
  }

  def checkId[A <: AnyRef](a: A): A = {
    val res = checkSer(a)
    res shouldBe theSameInstanceAs (a)
    res
  }

  def roundtripStr[A](a: A)(implicit A: Atomic[A]): A =
    A.fromString(A.stringRepr(a)).fold(err => fail(err.msg), a => a)

  def roundtripBin[A](a: A)(implicit A: Atomic[A]): A = {
    val (a2, r) = A.fromBinary(A.binaryRepr(a)).fold(err => fail(err.msg), ar => ar)
    r.length should === (0L)
    a2
  }
}

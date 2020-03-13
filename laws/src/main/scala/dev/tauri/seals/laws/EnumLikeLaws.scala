/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package laws

import cats.Eq
import cats.kernel.laws._
import cats.kernel.laws.discipline._
import cats.implicits._

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

import core.EnumLike

object EnumLikeLaws {
  def apply[A](implicit arb: Arbitrary[A], enu: EnumLike[A], equ: Eq[A]): EnumLikeLaws[A] = {
    new EnumLikeLaws[A] {
      def Arb = arb
      def Enu = enu
      def Equ = equ
    }
  }
}

trait EnumLikeLaws[A] extends Laws {

  implicit def Arb: Arbitrary[A]
  implicit def Enu: EnumLike[A]
  implicit def Equ: Eq[A]

  def name: this.RuleSet = new EnumLikeRuleSet(
    "name",
    parent = None,
    "name-fromName" -> forAll { a: A =>
      Enu.fromName(Enu.name(a)) <-> Right(a)
    },
    "fromName-name" -> forAll { s: String =>
      Enu.fromName(s).fold(
        _ => provedIsEq[String],
        a => Enu.name(a) <-> s
      )
    }
  )

  def index: this.RuleSet = new EnumLikeRuleSet(
    "index",
    parent = Some(name),
    "index-fromIndex" -> forAll { a: A =>
      Enu.fromIndex(Enu.index(a)) <-> Right(a)
    },
    "fromIndex-index" -> forAll { i: Int =>
      Enu.fromIndex(i).fold(
        _ => provedIsEq[Int],
        a => Enu.index(a) <-> i
      )
    }
  )

  def all: this.RuleSet = new EnumLikeRuleSet(
    "all",
    parent = Some(index)
  )

  final class EnumLikeRuleSet(
    val name: String,
    val parent: Option[this.RuleSet],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val bases = Nil
  }
}

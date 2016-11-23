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
package circe
package claws

import cats.kernel.laws._
import cats.kernel.Eq

import io.circe._
import io.circe.syntax._

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

object CirceLaws {
  def apply[A](implicit a: Arbitrary[A], e: Eq[A]): CirceLaws[A] = new CirceLaws[A] {
    def Arb = a
    def Equ = e
  }
}

trait CirceLaws[A] extends Laws {

  implicit def Arb: Arbitrary[A]
  implicit def Equ: Eq[A]

  def roundtrip(implicit enc: Encoder[A], dec: Decoder[A]): CirceRuleSet = new CirceRuleSet(
    name = "roundtrip",
    parent = None,
    "encode-then-decode" -> forAll { (a: A) =>
      a.asJson.as[A] match {
        case Right(r) =>
          r ?== a
        case Left(err) =>
          Prop(Result(status = False)) :| {
            err.toString
          }
      }
    }
  )

  final class CirceRuleSet(
    val name: String,
    val parent: Option[this.RuleSet],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val bases = Nil
  }
}

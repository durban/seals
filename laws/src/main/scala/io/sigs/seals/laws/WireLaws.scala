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
package laws

import cats.{ ~>, Eq }
import cats.kernel.laws._
import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

import io.sigs.seals.core.Wire

object WireLaws {

  def apply[A, R, E](wfr: Reified ~> Wire.Aux[?, R, E])(
    implicit
    arbA: Arbitrary[A],
    equA: Eq[A],
    reiA: Reified[A]
  ): WireLaws[A, R, E] = new WireLaws[A, R, E] {

    def ArbA: Arbitrary[A] = arbA
    def EquA: Eq[A] = equA
    def ReiA: Reified[A] = reiA

    def wireFromReified[X](implicit X: Reified[X]): Wire.Aux[X, R, E] = wfr(X)
  }
}

trait WireLaws[A, R, E] extends Laws with ArbInstances {

  implicit def ArbA: Arbitrary[A]
  implicit def EquA: Eq[A]
  implicit def ReiA: Reified[A]

  implicit def wireFromReified[X](implicit X: Reified[X]): Wire.Aux[X, R, E]

  def roundtrip: this.RuleSet = new WireRuleSet(
    "roundtrip",
    parent = None,
    "simpleRoundtrip" -> simpleRoundtrip[A],
    "envelopeSimpleRoundtrip" -> simpleRoundtrip[Envelope[A]],
    "modelSimpleRoundtrip" -> simpleRoundtrip[Model]
  )

  def roundtripCompat[B](
    implicit arbB: Arbitrary[B], equB: Eq[B], reiB: Reified[B], compat: Compat[A, B]
  ): this.RuleSet = new WireRuleSet(
    "roundtripCompat",
    parent = Some(roundtrip),
    "roundtripThroughCompat" -> roundtripThroughCompat[A, B],
    "envelopeRoundtripThroughCompat" -> roundtripThroughCompat[Envelope[A], Envelope[B]]
  )

  private def simpleRoundtrip[X](
    implicit arb: Arbitrary[X], wir: Wire.Aux[X, R, E], equ: Eq[X]
  ): Prop = {
    forAll { (x: X) =>
      wir.toWire(x).fold(
        err => Prop.proved,
        repr => {
          wir.fromWire(repr).fold(
            err => Prop(Result(status = False)) :| { err.toString },
            x2 => x2 ?== x)
        }
      )
    }
  }

  private def roundtripThroughCompat[X, Y](
    implicit
    arbX: Arbitrary[X],
    arbY: Arbitrary[Y],
    wirX: Wire.Aux[X, R, E],
    wirY: Wire.Aux[Y, R, E],
    equX: Eq[X],
    equY: Eq[Y]
  ): Prop = {
    forAll { (x: X, y: Y) =>
      val xy = wirX.toWire(x).fold(
        err => Prop.proved,
        repr => {
          wirY.fromWire(repr).fold(
            err => Prop(Result(status = False)) :| s"xy: ${err}",
            y2 => Prop.proved // TODO: should eq `x` with defaults
          )
        }
      )
      val yx = wirY.toWire(y).fold(
        err => Prop.proved,
        repr => {
          wirX.fromWire(repr).fold(
            err => Prop(Result(status = False)) :| s"yx: ${err}",
            x2 => Prop.proved // TODO: should eq `y` with defaults
          )
        }
      )

      xy && yx
    }
  }

  final class WireRuleSet(
    val name: String,
    val parent: Option[RuleSet],
    val props: (String, Prop)*
  ) extends RuleSet with HasOneParent {
    val bases = Nil
  }
}

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
package laws

import cats.{ ~>, Eq, Show }
import cats.implicits._

import org.typelevel.discipline.Laws
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Prop._

import core.{ Wire, CanonicalRepr }

object WireLaws {

  def apply[A, R, E](wfr: Reified ~> Wire.Aux[*, R, E])(
    implicit
    arbA: Arbitrary[A],
    equA: Eq[A],
    equR: Eq[R],
    reiA: Reified[A],
    shwA: Show[A],
    shwE: Show[E]
  ): WireLaws[A, R, E] = new WireLaws[A, R, E] {

    def ArbA: Arbitrary[A] = arbA
    def EquA: Eq[A] = equA
    def EquR: Eq[R] = equR
    def ReiA: Reified[A] = reiA
    def ShwA: Show[A] = shwA
    def ShwE: Show[E] = shwE

    def wireFromReified[X](implicit X: Reified[X]): Wire.Aux[X, R, E] = wfr(X)
  }
}

trait WireLaws[A, R, E] extends Laws with ArbInstances {

  implicit def ArbA: Arbitrary[A]
  implicit def EquA: Eq[A]
  implicit def EquR: Eq[R]
  implicit def ReiA: Reified[A]
  implicit def ShwA: Show[A]
  implicit def ShwE: Show[E]

  implicit def wireFromReified[X](implicit X: Reified[X]): Wire.Aux[X, R, E]

  def roundtrip: this.RuleSet = new WireRuleSet(
    "roundtrip",
    parent = None,
    "simpleRoundtrip" -> simpleRoundtrip[A],
    "envelopeSimpleRoundtrip" -> simpleRoundtrip[Envelope[A]],
    "modelSimpleRoundtrip" -> simpleRoundtrip[Model]
  )

  def roundtripCompat[B](
    implicit arbB: Arbitrary[B], equB: Eq[B], reiB: Reified[B], @proof compat: Compat[A, B]
  ): this.RuleSet = new WireRuleSet(
    "roundtripCompat",
    parent = Some(roundtrip),
    "roundtripThroughCompat" -> roundtripThroughCompat[A, B],
    "envelopeRoundtripThroughCompat" -> roundtripThroughCompat[Envelope[A], Envelope[B]]
  )

  private def simpleRoundtrip[X](
    implicit arb: Arbitrary[X], wir: Wire.Aux[X, R, E], equ: Eq[X], shw: Show[X]
  ): Prop = {
    forAll { (x: X) =>
      wir.toWire(x).fold(
        _ => Prop.proved,
        repr => {
          wir.fromWire(repr).fold(
            err => Prop.falsified :| sh"cannot decode encoded value '${x}': '${err}'",
            x2 => {
              val objOk = x2 === x
              wir.toWire(x2).fold(
                err => Prop.falsified :| sh"cannot reencode decoded value '${x2}': '${err}'",
                repr2 => {
                  val reprOk = repr2 === repr
                  Prop(objOk && reprOk)
                }
              )
            }
          )
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
        _ => Prop.proved,
        repr => {
          wirY.fromWire(repr).fold(
            err => Prop.falsified :| sh"xy: ${err}",
            y2 => {
              CanonicalRepr.unfold[Y](
                CanonicalRepr.fold[X](x)(wirX.reified)
              )(wirY.reified).fold(
                { err => Prop.falsified :| sh"xyx: ${err}" },
                { transformed: Y => Prop(y2 === transformed) }
              )
            }
          )
        }
      )
      val yx = wirY.toWire(y).fold(
        _ => Prop.proved,
        repr => {
          wirX.fromWire(repr).fold(
            err => Prop.falsified :| sh"yx: ${err}",
            x2 => {
              CanonicalRepr.unfold[X](
                CanonicalRepr.fold[Y](y)(wirY.reified)
              )(wirX.reified).fold(
                { err => Prop.falsified :| sh"yxy: ${err}" },
                { transformed: X => Prop(x2 === transformed) }
              )
            }
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

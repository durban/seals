/*
 * Copyright 2016-2017 Daniel Urban
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
package test

import org.scalatest.{ FlatSpec, Matchers }

import shapeless.Witness

@schema
final case class Foo(i: Int, s: String)

@schema
sealed trait ST
final case class X(i: Int) extends ST
object ST // SI-7046 workaround

object A {

  @schema
  final case class Foo(i: Int, f: Float)

  @schema
  sealed trait ST
  final case class X(b: Boolean) extends ST
  object ST // SI-7046 workaround

  object Bar {
    final val constModel = "bar"
  }

  object Baz {
    final val constModel: Witness.`"baz"`.T = "baz"
  }
}

class SchemaSpec extends FlatSpec with Matchers {

  "Extract" should "work" in {
    import SchemaExtractor.extract
    extract("io.sigs.seals.test.A.Bar", "constModel") should === ("bar")
    extract("io.sigs.seals.test.A.Baz", "constModel") should === ("baz")
  }

  "@schema" should "put a val and a def into the companion object" in {
    val inst = Foo.$io$sigs$seals$core$Reified$Instance
    inst shouldBe theSameInstanceAs (
      Foo.$io$sigs$seals$core$Reified$Instance$Forwarder()
    )
    inst.model should === (Reified[Foo].model)
    ST.$io$sigs$seals$core$Reified$Instance.model should === (Reified[ST].model)

    A.Foo.$io$sigs$seals$core$Reified$Instance.model should === (Reified[A.Foo].model)
    A.ST.$io$sigs$seals$core$Reified$Instance.model should === (Reified[A.ST].model)
  }
}

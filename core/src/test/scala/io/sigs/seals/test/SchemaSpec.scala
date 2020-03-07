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

package io.sigs.seals
package test

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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

class SchemaSpec extends AnyFlatSpec with Matchers {

  "Extract" should "work" in {
    import SchemaExtractor.extract
    extract("io.sigs.seals.test.A.Bar", "constModel") should === ("bar")
    extract("io.sigs.seals.test.A.Baz", "constModel") should === ("baz")
  }

  "@schema" should "put a cached implicit val into the companion object" in {
    val inst = Foo.reifiedFoo
    inst shouldBe theSameInstanceAs (implicitly[Reified[Foo]])
    inst.model should === (Reified[Foo].model)

    ST.reifiedST.model should === (Reified[ST].model)
    ST.reifiedST shouldBe theSameInstanceAs (implicitly[Reified[ST]])

    A.Foo.reifiedFoo.model should === (Reified[A.Foo].model)
    A.ST.reifiedST.model should === (Reified[A.ST].model)
  }
}

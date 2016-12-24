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
package core
package test

import org.scalatest.{ FlatSpec, Matchers }

import shapeless.Witness

object A {

  @schema
  final case class Foo()

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
    extract("io.sigs.seals.core.test.A.Foo", "constModel") should === ("Model[HNil]")
    extract("io.sigs.seals.core.test.A.Bar", "constModel") should === ("bar")
    extract("io.sigs.seals.core.test.A.Baz", "constModel") should === ("baz")
  }
}

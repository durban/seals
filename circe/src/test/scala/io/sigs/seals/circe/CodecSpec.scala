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

import java.util.UUID

import cats.data.Xor

import io.circe._
import io.circe.syntax._

import Codec._
import laws.MyUUID
import laws.TestInstances.atomic.atomicMyUUID

object CodecSpec {

  final case class FooBarU(fb: FooBar, u: MyUUID)

  sealed trait FooBar extends Product with Serializable
  final case class Foo(a: Int, b: String = "ert") extends FooBar
  final case object Bar extends FooBar

  def fooJson(a: Int, b: String = "ert"): Json = {
    Json.obj(
      "a" -> Json.obj(
        "value" -> Json.fromString(a.toString)
      ),
      "b" -> Json.obj(
        "value" -> Json.fromString(b)
      )
    )
  }

  def fooBarJson(foo: Option[(Int, String)]): Json = {
    foo.fold {
      Json.obj("Bar" -> Json.obj())
    } { case (a, b) =>
      Json.obj("Foo" -> fooJson(a, b))
    }
  }
}

class CodecSpec extends BaseJsonSpec {

  import CodecSpec._

  "Encoding" - {

    "Case classes" in {
      val foo: Foo = Foo(5)
      foo.asJson should === (fooJson(5))
    }

    "Case objects" in {
      Bar.asJson should === (Json.obj())
    }

    "ADTs" in {
      val foo: FooBar = Foo(a = 42, b = "abc")
      foo.asJson should === (fooBarJson(Some((42, "abc"))))

      val bar: FooBar = Bar
      bar.asJson should === (
        Json.obj(
          "Bar" -> Json.obj()
        )
      )
    }
  }

  "Decoding" - {

    "Case classes" in {
      fooJson(a = 5, b = "xyz").as[Foo] should === (
        Xor.right(Foo(a = 5, b = "xyz"))
      )
    }

    "Optional fields" in {
      val json = Json.obj(
        "a" -> Json.obj(
          "value" -> Json.fromString(89.toString)
        )
      )
      json.as[Foo] should === (
        Xor.right(Foo(a = 89))
      )

      val json2 = Json.obj(
        "Foo" -> json
      )
      json2.as[FooBar] should === (
        Xor.right(Foo(a = 89) : FooBar)
      )
    }

    "Case objects" in {
      Json.obj().as[Bar.type] should === (Xor.right(Bar))
    }

    "ADTs" in {
      fooBarJson(Some((42, "abc"))).as[FooBar] should === (
        Xor.right(Foo(42, "abc") : FooBar)
      )
      fooBarJson(None).as[FooBar] should === (
        Xor.right(Bar : FooBar)
      )
    }

    "Field order shouldn't matter" in {
      val json = Json.obj(
        "Foo" -> Json.obj(
          "b" -> Json.obj(
            "value" -> Json.fromString("pqrst")
          ),
          "a" -> Json.obj(
            "value" -> Json.fromString(99.toString)
          )
        )
      )
      json.as[FooBar] should === (Xor.right(Foo(99, "pqrst") : FooBar))
    }
  }

  "Roundtrip" - {
    val x = FooBarU(Foo(42), MyUUID(UUID.fromString("69fd9ed5-4789-4290-b55c-f5f1a773265a")))
    val j = x.asJson
    val y = j.as[FooBarU]
    y should === (Xor.right(x))
  }
}

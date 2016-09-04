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

import cats.data.Xor

import io.circe._

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

class JsonRefSpec extends BaseJsonSpec with GeneratorDrivenPropertyChecks {

  "JsonRef from URI string" - {

    "simple fragment" in {
      JsonRef("#/foo/bar").map(_.path) should === (Xor.right(Vector("foo", "bar")))
    }

    "full URI disallowed" in {
      JsonRef("http://example.com#/foo") should be ('left)
    }

    "unescaping" in {
      JsonRef("#/fo~1o/ba~0r").map(_.path) should === (Xor.right(Vector("fo/o", "ba~r")))
    }

    "empty keys" in {
      JsonRef("#//").map(_.path) should === (Xor.right(Vector("", "")))
    }
  }

  "JsonRef to URI string" - {

    "simple" in {
      JsonRef(Vector("foo", "bar")).uri should === ("#/foo/bar")
    }

    "escaping" in {
      JsonRef(Vector("fo/o", "ba~r")).uri should === ("#/fo~1o/ba~0r")
    }
  }

  "Roundtrip" in {

    val paths = Gen.listOf(
      Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('/'))
    ).map(_.mkString)

    forAll (paths) { s =>
      val uri = "#/" + s
      val ref = JsonRef(uri)
      whenever(ref.isRight) {
        ref.map { r =>
          r.uri should === (uri)
        }
      }
    }
  }

  "JSON repr" - {

    "encoding" in {
      JsonRef(Vector("foo/", "bar~")).asJson should === (
        Json.obj(s"$$ref" -> Json.fromString("#/foo~1/bar~0"))
      )
    }

    "decoding" in {
      val json = Json.obj(
        s"$$ref" -> Json.fromString("#/~0foo/~1bar"),
        "dummy" -> Json.arr(Json.fromString("boo"), Json.fromInt(56))
      )
      json.as[JsonRef] should === (Xor.right(JsonRef(Vector("~foo", "/bar"))))
    }

    "roundtrip" in {
      val paths = Gen.listOf(Gen.alphaStr).map(_.toVector)
      forAll (paths) { v =>
        val ref1 = JsonRef(v)
        val ref2 = ref1.asJson.as[JsonRef].getOrElse(fail("failed decoding"))
        ref1.path should === (ref2.path)
      }
    }
  }
}

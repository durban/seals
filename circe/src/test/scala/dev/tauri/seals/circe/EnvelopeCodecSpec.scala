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

package dev.tauri.seals
package circe

import cats.implicits._

import io.circe._
import io.circe.syntax._

import Codecs._

object EnvelopeCodecSpec {

  final case class Foo(i: Int)

  final case class Foo2(i: Int, s: String)
}

class EnvelopeCodecSpec extends BaseJsonSpec {

  import EnvelopeCodecSpec._

  "Roundtrip" in {
    val json: Json = Envelope[Foo](Foo(42)).asJson
    json.as[Envelope[Foo]] should === (Either.right(Envelope[Foo](Foo(42))))
  }

  "Incompatible models" in {
    val json: Json = Envelope[Foo2](Foo2(42, "ert")).asJson
    val errPat = ".*incompatible models.*".r
    inside (json.as[Envelope[Foo]]) {
      case Left(DecodingFailure(errPat(), _)) => // OK
      case x: Any => fail(s"unexpected: ${x}")
    }
  }
}

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

import io.circe._
import io.circe.syntax._
import org.scalatest.matchers.{ Matcher, MatchResult }
import scala.Vector

trait BaseJsonSpec extends tests.BaseSpec {

  def roundtripJson[A : Encoder : Decoder](a: A): A = {
    val json: Json = a.asJson
    json.as[A].fold(
      f => fail(f.toString),
      r => r
    )
  }

  def checkJson[A : Encoder : Decoder](a: A): A = {
    val res: A = roundtripJson(a)
    res should === (a)
    a should === (res)
    a.## should === (res.##)
    res
  }

  def checkJsonId[A <: AnyRef : Encoder : Decoder](a: A): A = {
    val res = checkJson(a)
    res shouldBe theSameInstanceAs (a)
    res
  }

  // TODO: also check cursor history
  def failWith(msg: String = ".*"): Matcher[Decoder.Result[Model]] = Matcher { res =>
    res.fold(
      failure => MatchResult(
        failure.message.matches(msg),
        "actual error message {0} does not match {1}",
        "actual error message {0} matches {1}", // FIXME
        args = Vector(failure.message, msg)
      ),
      success => MatchResult(
        false,
        "a decoding success (with result {0}) is not a failure with message {1}",
        "a decoding success (with result {0}) is a failure with message {1}", // FIXME
        args = Vector(success, msg)
      )
    )
  }
}

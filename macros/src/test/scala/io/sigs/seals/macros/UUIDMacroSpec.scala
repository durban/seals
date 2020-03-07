/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package macros

import java.util.UUID

import shapeless.test.{ typed, illTyped }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals

class UUIDMacroSpec extends AnyFlatSpec with Matchers with TypeCheckedTripleEquals {

  def uuid: Nothing = sys.error("this should never be invoked")

  "UUID literal macro" should "work" in {
    val x = uuid"3a108ce3-9284-4883-a72c-71ced1f30e4c"
    typed[UUID] { x }
    x.variant should === (2)
    x.version should === (4)
    x should === (UUID.fromString("3a108ce3-9284-4883-a72c-71ced1f30e4c"))
  }

  it should "reject non-RFC-4122 UUIDs" in {
    val str = "00000000-0000-0001-0000-000000000002"
    val u = UUID.fromString(str)
    u.variant should !== (2)
    illTyped(
      """uuid"00000000-0000-0001-0000-000000000002"""",
      ".*not an RFC-4122 UUID.*"
    )
  }

  it should "reject syntactically invalid strings" in {
    illTyped(
      """uuid"abcd"""",
      ".*not a valid UUID.*"
    )
  }

  it should "only accept string literals" in {
    illTyped(
      """
        val v: String = "3a108ce3-9284-4883-a72c-71ced1f30e4c"
        StringContext(v).uuid()
      """,
      ".*not a string literal.*"
    )
  }
}

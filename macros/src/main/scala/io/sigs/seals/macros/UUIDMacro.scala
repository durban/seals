/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

import scala.reflect.macros.blackbox.Context

import cats.implicits._

object UUIDMacro extends StringInterpolatorHelper {

  def impl(c: Context)(): c.Expr[UUID] = {
    import c.universe._
    val s: String = extractLiteral(c)
    val u: UUID = try {
      UUID.fromString(s)
    } catch {
      case ex: IllegalArgumentException =>
        c.abort(c.enclosingPosition, show"not a valid UUID (${ex.getMessage})")
    }
    if (u.variant =!= 2) {
      c.abort(c.enclosingPosition, "not an RFC-4122 UUID (variant is not 2)")
    }
    val msb: Long = u.getMostSignificantBits
    val lsb: Long = u.getLeastSignificantBits
    c.Expr[UUID](q"new _root_.java.util.UUID(${msb}, ${lsb})")
  }
}

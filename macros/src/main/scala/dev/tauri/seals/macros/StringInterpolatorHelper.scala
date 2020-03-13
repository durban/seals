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

package dev.tauri.seals
package macros

import scala.reflect.macros.blackbox.Context

trait StringInterpolatorHelper {

  def extractLiteral(c: Context): String = {
    import c.universe._
    val s: String = c.prefix.tree match {
      case Apply(_, List(Apply(_, List(Literal(Constant(s: String)))))) =>
        s
      case _ =>
        c.abort(c.enclosingPosition, "not a string literal")
    }
    s
  }
}


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
package checker

@schema
sealed trait Foo
final case class Bar(i: Int) extends Foo
final case object Baz extends Foo
object Foo // SI-7046 workaround

@schema
final case class CC(i: Int, s: String)

object Wrap {

  @schema
  sealed trait WFoo
  final case class Bar(j: Float) extends WFoo
  final case object Baz extends WFoo
  object WFoo // SI-7046 workaround

  @schema
  final case class WCC(s: String)
}

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

package com.example.test1

import dev.tauri.seals.schema

object Test1 {

  @schema
  sealed trait Message

  final case class Put(k: String, v: Int) extends Message

  final case class Get(k: String) extends Message

  object Message // SI-7046 workaround

  @schema
  final case class Response(k: String, code: Int)
}

object Test1b {

  @schema
  final case class Foo(i: Int)
}

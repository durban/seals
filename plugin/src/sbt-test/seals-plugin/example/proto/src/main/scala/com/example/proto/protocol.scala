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

package com.example.proto

import dev.tauri.seals.schema

@schema
sealed trait Request

final case class RandomNumber(min: Int, max: Int)
  extends Request

final case class ReSeed(n: Long)
  extends Request

object Request // SI-7046 workaround


@schema
sealed trait Response

final case class Number(value: Int)
  extends Response

final case object Ok
  extends Response

object Response // SI-7046 workaround

/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
 * Copyright 2020 Nokia
 * SPDX-License-Identifier: Apache-2.0
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

package dev.tauri.seals.tests // Note: intentionally not nested package

import java.util.UUID

class UUIDUtilsSpec extends BaseSpec {

  import dev.tauri.seals.core.UUIDUtils._

  val u = uuid"26740e0a-445e-4d60-93f6-df7bf72329c1"

  "Literal syntax" in {
    u : UUID
  }

  "Builder syntax" in {
    val u2 = (u / "foo" / "bar").uuid
    u2 : UUID
  }
}

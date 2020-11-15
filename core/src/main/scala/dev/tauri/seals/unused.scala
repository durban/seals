/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

package dev.tauri.seals

/** Annotation for intentionally unused parameters */
private[seals] final class unused
  extends scala.annotation.nowarn("msg=parameter value .* never used")

/** Annotation for parameters which are "unused", but required as proof/evidence */
private[seals] final class proof
  extends scala.annotation.nowarn("msg=parameter value .* never used")

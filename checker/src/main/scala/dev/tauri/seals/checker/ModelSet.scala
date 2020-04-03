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
package checker

import io.circe.{ Encoder, Decoder }

import circe.Codecs

/** A newtype for fine-grained control over the JSON format */
final case class ModelSet(models: Map[String, Map[String, Model]])

final object ModelSet {

  implicit val encoderInstance: Encoder[ModelSet] = {
    // Note: we're explicitly only using `Reified` for decoding `Model`,
    // otherwise we want the default behavior of circe.
    implicit val mEnc: Encoder[Model] = Codecs.encoderFromReified[Model]
    Encoder[Map[String, Map[String, Model]]].contramap(_.models)
  }

  implicit val decoderInstance: Decoder[ModelSet] = {
    // Note: we're explicitly only using `Reified` for encoding `Model`,
    // otherwise we want the default behavior of circe.
    implicit val mDec: Decoder[Model] = Codecs.decoderFromReified[Model]
    Decoder[Map[String, Map[String, Model]]].map(ModelSet(_))
  }
}

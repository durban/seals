/*
 * Copyright 2016 Daniel Urban
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
package circe

import io.circe._
import io.circe.syntax._

import cats.data.Xor

// TODO: tests!

object EnvelopeCodec {

  implicit def envelopeEncoder[A]: ObjectEncoder[Envelope[A]] = new ObjectEncoder[Envelope[A]] {
    override def encodeObject(a: Envelope[A]): JsonObject = {
      ("model" -> a.model.asJson(ModelCodec.modelEncoder)) +:
      ("value" -> a.value.asJson(Codec.encoderFromReified(a.reified))) +:
      JsonObject.empty
    }
  }

  implicit def envelopeDecoder[A](implicit r: Reified[A]): Decoder[Envelope[A]] = new Decoder[Envelope[A]] {
    override def apply(cur: HCursor): Decoder.Result[Envelope[A]] = {
      for {
        model <- cur.get[Model]("model")(ModelCodec.modelDecoder(r.model))
        value <- if (model compatible r.model) {
          cur.get[A]("value")(Codec.decoderFromReified(r))
        } else {
          Xor.left(DecodingFailure(
            s"incompatible models: expected '${r.model}', got '${model}'",
            cur.history
          ))
        }
      } yield Envelope(value)(r)
    }
  }
}

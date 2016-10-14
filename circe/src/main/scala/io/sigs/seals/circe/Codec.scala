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

import cats.data.Xor

import io.circe._

object Codec {

  // TODO: these also provide codecs for String, Int, ...
  // (every Atom), and when imported, they have a higher
  // priority than the default codecs in circe

  implicit def encoderFromReified[A](implicit A: Reified[A]): Encoder[A] = new Encoder[A] {
    override def apply(a: A): Json = {
      val obj = A.genFold[Json, JsonObject](a)(
        atom = a => Json.fromString(a),
        hNil = () => JsonObject.empty,
        hCons = (l, h, t) => (l.name, h) +: t,
        prod = Json.fromJsonObject,
        sum = (l, v) => Json.obj(l.name -> v),
        vector = v => Json.arr(v: _*)
      )
      A.close(obj, Json.fromJsonObject)
    }
  }

  implicit def decoderFromReified[A](implicit A: Reified[A]): Decoder[A] = new Decoder[A] {
    override def apply(c: HCursor): Decoder.Result[A] = {
      A.unfold[HCursor, DecodingFailure](
        atom = _.as[String](Decoder.decodeString),
        atomErr = { cur =>
          DecodingFailure(s"cannot decode atom", cur.history)
        },
        hNil = _.as[JsonObject](Decoder.decodeJsonObject).map(_ => ()),
        hCons = { (cur, sym) =>
          cur.downField(sym.name).either.leftMap { fc =>
            DecodingFailure(s"missing key: '${sym.name}'", fc.history)
          }.map { hc =>
            Xor.right[DecodingFailure, (HCursor, HCursor)]((hc, cur))
          }
        },
        cNil = { cur =>
          DecodingFailure("no variant matched (CNil reached)", cur.history)
        },
        cCons = { (cur, sym) =>
          cur.downField(sym.name).either.fold(
            fc => Xor.right(Right(cur)),
            sc => Xor.right(Left(sc))
          )
        },
        vector = { cur =>
          cur.as[Vector[HCursor]]
        }
      )(c)
    }
  }
}

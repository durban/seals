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
      val obj = A.fold[JsonObject](a)(
        atom = a => JsonObject.singleton("value", Json.fromString(a)),
        hNil = () => JsonObject.empty,
        hCons = (l, h, t) => (l.name, Json.fromJsonObject(h)) +: t,
        sum = (l, v) => JsonObject.singleton(l.name, Json.fromJsonObject(v)),
        // TODO: try to avoid extra singleton object
        vector = v => JsonObject.singleton("Vector", Json.arr(v.map(Json.fromJsonObject): _*))
      )
      Json.fromJsonObject(obj)
    }
  }

  implicit def decoderFromReified[A](implicit A: Reified[A]): Decoder[A] = new Decoder[A] {
    override def apply(c: HCursor): Decoder.Result[A] = {
      A.unfold[HCursor, DecodingFailure](
        atom = { cur =>
          for {
            vc <- cur.downField("value").either.leftMap { fc =>
              DecodingFailure("missing key: 'value'", fc.history)
            }
            vs <- vc.as[String](Decoder.decodeString)
          } yield vs
        },
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
          for {
            c <- cur.downField("Vector").either.leftMap { fc =>
              DecodingFailure("missing key: 'Vector'", fc.history)
            }
            vect <- c.as[Vector[HCursor]]
          } yield vect
        }
      )(c)
    }
  }
}

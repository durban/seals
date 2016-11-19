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
import cats.implicits._

// TODO: rename to Codecs
object Codec {

  // TODO: these also provide codecs for String, Int, ...
  // (every Atom), and when imported, they have a higher
  // priority than the default codecs in circe

  implicit def encoderFromReified[A](implicit A: Reified[A]): Encoder[A] = new Encoder[A] {
    override def apply(a: A): Json = {
      val obj = A.fold(a)(Reified.Folder.instance[Json, JsonObject](
        atom = a => Json.fromString(a),
        hNil = () => JsonObject.empty,
        hCons = (l, h, t) => (l.name, h) +: t,
        prod = Json.fromJsonObject,
        sum = (l, v) => Json.obj(l.name -> v),
        vector = v => Json.arr(v: _*)
      ))
      A.close(obj, Json.fromJsonObject)
    }
  }

  implicit def decoderFromReified[A](implicit A: Reified[A]): Decoder[A] = new Decoder[A] {
    override def apply(c: HCursor): Decoder.Result[A] = {
      val x = A.unfold(Reified.Unfolder.instance[HCursor, DecodingFailure, (Boolean, HCursor)](
        atom = { cur => cur.as[String](Decoder.decodeString).map(s => (s, cur)) },
        atomErr = { cur =>
          DecodingFailure(s"cannot decode atom", cur.history)
        },
        hNil = { cur => cur.as[JsonObject](Decoder.decodeJsonObject).map(_ => cur) },
        hCons = { (cur, sym) =>
          cur.downField(sym.name).either.leftMap { fc =>
            DecodingFailure(s"missing key: '${sym.name}'", fc.history)
          }.map { hc =>
            Either.right((hc, (_: HCursor) => Either.right(cur)))
          }
        },
        cNil = { cur =>
          DecodingFailure("no variant matched (CNil reached)", cur.history)
        },
        cCons = { (cur, sym) =>
          cur.downField(sym.name).either.fold(
            fc => Either.right(Right(cur)),
            sc => Either.right(Left(sc))
          )
        },
        vectorInit = { cur =>
          for {
            // just to make sure it's an array:
            _ <- cur.as[Vector[HCursor]].leftMap { cur =>
              DecodingFailure("not an array", cur.history)
            }
          } yield {
            // TODO: this is a workaround - we preserve
            // the cursor in the state, because the one
            // passed on by `unfold` won't always be correct
            // (the `unfold` signature is not general enough,
            // for JSON we'd need something to move up the
            // cursor after decoding, e.g., a CCons).
            (cur, (true, cur))
          }
        },
        vectorFold = { case (_, (first, cur)) =>
          val cur2 = if (first) cur.downArray else cur.right
          cur2.either.map(x => Option((x, (false, x)))).recover {
            case _ => None
          }.leftMap { cur =>
            DecodingFailure("not an array", cur.history)
          }
        },
        unknownError = { msg => DecodingFailure(msg, Nil) }
      ))(c)

      x.map { case (a, _) => a }
    }
  }
}

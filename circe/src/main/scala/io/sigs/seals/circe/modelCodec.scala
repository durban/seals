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

import cats.data.{ State, Xor, XorT }
import cats.{ Eval, Monad }

import io.circe._

object ModelCodec
  extends ModelEncoder
  with ModelDecoder

trait ModelEncoder {

  private type PreSt = State[Map[Model, Model.Path], Unit]

  // TODO: inject version number
  implicit val modelEncoder: ObjectEncoder[Model] = new ObjectEncoder[Model] {
    override def encodeObject(a: Model): JsonObject = {
      val pre = a.foldC[PreSt](
        hNil = _ => State.pure(()),
        hCons = (c, l, _, h, t) => compositePre("HCons", c, l, h, t),
        cNil = _ => State.pure(()),
        cCons = (c, l, h, t) => compositePre("CCons", c, l, h, t),
        vector = (c, e) => State.pure(()),
        atom = (_, a) => State.pure(()),
        cycle = _ => State.pure(())
      )
      val map = pre.runS(Map.empty).value
      a.foldC[JsonObject](
        hNil = _ => JsonObject.singleton("HNil", Json.obj()),
        hCons = (c, l, o, h, t) => composite("HCons", l, Some(o), h, t),
        cNil = _ => JsonObject.singleton("CNil", Json.obj()),
        cCons = (c, l, h, t) => composite("CCons", l, None, h, t),
        vector = (c, e) => JsonObject.singleton(
          "Vector",
          Json.obj("elem" -> Json.fromJsonObject(e))
        ),
        atom = (_, a) => JsonObject.singleton(
          "Atom",
          Json.obj("id" -> Json.fromString(a.uuid.toString))
        ),
        cycle = { c =>
          val path = map.get(c.m).getOrElse {
            throw new IllegalStateException(
              s"no path found for cycle at ${c.p} (map is ${map})"
            )
          }

          // last component is a tag, we don't need that
          mkRef(path.dropRight(1))
        }
      )
    }
  }

  private def composite(
    label: String,
    l: Symbol,
    optional: Option[Boolean],
    h: JsonObject,
    t: JsonObject
  ) = {
    val opt = optional.map(
      b => Vector("optional" -> Json.fromBoolean(b))
    ).getOrElse(Vector.empty)
    val fields = Vector(
      "label" -> Json.fromString(l.name)
    ) ++ opt ++ Vector(
      "head" -> Json.fromJsonObject(h),
      "tail" -> Json.fromJsonObject(t)
    )
    JsonObject.singleton(
      label,
      Json.obj(fields: _*)
    )
  }

  private def compositePre(
    label: String,
    c: Model.Ctx,
    l: Symbol,
    h: PreSt,
    t: PreSt
  ): PreSt = {
    for {
      _ <- h
      _ <- t
      _ <- State.modify[Map[Model, Model.Path]](_ + (c.m -> c.p))
    } yield ()
  }

  private def mkRef(path: Model.Path): JsonObject = {
    JsonRef(path).asJsonObject
  }
}

trait ModelDecoder {

  // TODO: only use lazy val where necessary

  private type DecSt[A] = XorT[StateHelper, DecodingFailure, A]

  // TODO: consider using kind projector
  private type StateHelper[A] = State[DecMap, A]

  private type DecMap = Map[Model.Path, Model]

  implicit def modelDecoder(implicit r: AtomRegistry): Decoder[Model] = Decoder.instance { cur =>
    cur.as[DecSt[Model]](decodeModel).flatMap { st =>
      st.value.runA(Map()).value.map { model =>
        // do a full traverse, to allow
        // dropping thunks and to ensure
        // that any bug in the decoding
        // will cause an early failure:
        val _ = model.desc

        model
      }
    }
  }

  private def decodeModel(implicit r: AtomRegistry): Decoder[DecSt[Model]] = {
    for {
      obj <- Decoder.decodeJsonObject
      res <- {
        if (obj.contains("HNil")) decodeHNil.prepare(_.downField("HNil")).map(_.map[Model](identity))
        else if (obj.contains("CNil")) decodeCNil.prepare(_.downField("CNil")).map(_.map[Model](identity))
        else if (obj.contains("Atom")) decodeAtom.prepare(_.downField("Atom")).map(_.map[Model](identity))
        else if (obj.contains("HCons")) decodeHCons.prepare(_.downField("HCons")).map(_.map[Model](identity))
        else if (obj.contains("CCons")) decodeCCons.prepare(_.downField("CCons")).map(_.map[Model](identity))
        else if (obj.contains("Vector")) decodeVector.prepare(_.downField("Vector")).map(_.map[Model](identity))
        else if (obj.contains(JsonRef.key)) decodeRef
        else Decoder.failed[DecSt[Model]](DecodingFailure("not a Model", Nil))
      }
    } yield res
  }

  private def decodeHList(implicit r: AtomRegistry): Decoder[DecSt[Model.HList]] = {
    for {
      obj <- Decoder.decodeJsonObject
      res <- {
        if (obj.contains("HNil")) decodeHNil.prepare(_.downField("HNil"))
        else if (obj.contains("HCons")) decodeHCons.prepare(_.downField("HCons"))
        else Decoder.failed[DecSt[Model.HList]](DecodingFailure("not a HList", Nil))
      }
    } yield res
  }

  private def decodeCoproduct(implicit r: AtomRegistry): Decoder[DecSt[Model.Coproduct]] = {
    for {
      obj <- Decoder.decodeJsonObject
      res <- {
        if (obj.contains("CNil")) decodeCNil.prepare(_.downField("CNil"))
        else if (obj.contains("CCons")) decodeCCons.prepare(_.downField("CCons"))
        else Decoder.failed[DecSt[Model.Coproduct]](DecodingFailure("not a Coproduct", Nil))
      }
    } yield res
  }

  private lazy val decodeHNil = liftDecoder[Model.HList](
    Decoder[Unit].map[Model.HList](_ => Model.HNil)
  )

  private lazy val decodeCNil = liftDecoder[Model.Coproduct](
    Decoder[Unit].map[Model.Coproduct](_ => Model.CNil)
  )

  private def decodeAtom(implicit r: AtomRegistry): Decoder[DecSt[Atom[_]]] = {
    val dec: Decoder[Atom[_]] = Decoder.instance { cur =>
      cur.get[String]("id")(Decoder.decodeString).flatMap { id =>
        r.getAtom(id).bimap(
          msg => DecodingFailure(msg, cur.downField("id").history),
          identity
        )
      }
    }
    liftDecoder[Atom[_]](dec)
  }

  private def decodeComposite[C <: Model, T <: Model](
    decTail: Decoder[DecSt[T]],
    mkObj: (Symbol, Boolean, Eval[Decoder.Result[Model]], Eval[Decoder.Result[T]]) => C,
    decodeOptional: Boolean
  )(implicit r: AtomRegistry): Decoder[DecSt[C]] = Decoder.instance { cur =>
    for {
      label <- cur.get[String]("label")
      head <- cur.get[DecSt[Model]]("head")(decodeModel)
      tail <- cur.get[DecSt[T]]("tail")(decTail)
      optional <- if (decodeOptional) {
        cur.get[Json]("optional").recover {
          // missing key, default to false:
          case _ => Json.False
        }.flatMap(j => j.as[Boolean])
      } else {
        Xor.right(false)
      }
    } yield {
      val path = JsonRef.fromCursor(cur.cursor).dropRight(1).path
      // TODO: try to clean this up
      val x = for {
        st <- State.get[DecMap]
      } yield {
        lazy val res: C = mkObj(Symbol(label), optional, laterHead, laterTail)
        lazy val newSt: DecMap = st + (path -> res)
        lazy val h: Eval[(DecMap, Decoder.Result[Model])] = head.value.run(newSt)
        lazy val laterHead = Eval.later(h.value._2)
        lazy val t: Eval[(DecMap, Decoder.Result[T])] = tail.value.run(h.value._1)
        lazy val laterTail = Eval.later(t.value._2)
        (res, t.value._1, (laterHead, laterTail))
      }

      XorT {
        x.flatMap {
          case (c, map, (h, t)) =>
            // now check whether decoding
            // `h` and `t` succeeds:
            val res = for {
              _ <- h.value
              _ <- t.value
            } yield c

            State.set(map).flatMap { _ =>
              res.fold(
                failure => {
                  // either `h` or `t` failed, so we
                  // must remove the (invalid) inserted
                  // composite instance:
                  State.modify[DecMap](_ - path).map(_ => res)
                },
                _ => State.pure(res)
              )
            }
        }
      }
    }
  }

  private def decodeHCons(implicit r: AtomRegistry): Decoder[DecSt[Model.HList]] = {
    decodeComposite[Model.HList, Model.HList](
      decodeHList,
      (l, o, h, t) => Model.HCons(
        l,
        o,
        h.value.getOrElse(core.impossible("accessing HCons parent of invalid `h`")),
        t.value.getOrElse(core.impossible("accessing HCons parent of invalid `t`"))
      ),
      decodeOptional = true
    )
  }

  private def decodeCCons(implicit r: AtomRegistry): Decoder[DecSt[Model.Coproduct]] = {
    decodeComposite[Model.Coproduct, Model.Coproduct](
      decodeCoproduct,
      (l, _, h, t) => Model.CCons(
        l,
        h.value.getOrElse(core.impossible("accessing CCons parent of invalid `h`")),
        t.value.getOrElse(core.impossible("accessing CCons parent of invalid `t`"))
      ),
      decodeOptional = false
    )
  }

  private def decodeVector(implicit r: AtomRegistry): Decoder[DecSt[Model.Vector]] = {
    Decoder.instance { cur =>
      for {
        e <- cur.get[DecSt[Model]]("elem")(decodeModel)
      } yield e.map(e => Model.Vector(e))
    }
  }

  private def decodeRef(implicit r: AtomRegistry): Decoder[DecSt[Model]] = Decoder.instance { cur =>
    cur.as[JsonRef].map { ref =>
      // TODO: use the implicit (needs kind projector?)
      XorT.catsDataTransLiftForXorT.liftT(State.get[DecMap]).flatMap { map =>
        // TODO: getOrElse
        map.get(ref.path).fold[DecSt[Model]] {
          XorT.left(State.pure(DecodingFailure(
            s"invalid backreference: ${ref.uri}",
            cur.history
          )))
        } { found =>
          XorT.right(State.pure(found))
        }
      }
    }
  }

  private def liftDecoder[A](decoder: Decoder[A]): Decoder[DecSt[A]] = {
    decoder.flatMap { a =>
      Monad[Decoder].pure(
        XorT.right(
          State.pure[DecMap, A](a)
        )
      )
    }
  }
}

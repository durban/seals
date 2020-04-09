/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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
package circe

import cats.implicits._

import io.circe._
import io.circe.syntax._

import core.Refinement
import laws.MyUUID
import laws.TestInstances
import laws.TestTypes

import Codecs._

object ModelCodecSpec {

  sealed trait Adt

  final case class C(i: Int, s: String) extends Adt
  object C {
    val refinement: Refinement.Aux[C, C] = new Refinement[C] {
      override type Repr = C
      override val uuid = uuid"52dbf550-fe9f-49a4-9ce3-519005b9ad5f"
      override val repr = Refinement.ReprFormat("α", true, "ω")
      override def from(c: C): Either[String, C] = {
        if (c.s === c.i.toString) Right(c)
        else Left("`s` is invalid")
      }
      override def to(c: C): C = c
    }
  }

  final case object D extends Adt

  sealed trait CyAdt
  final case class X(m: CyAdt) extends CyAdt
  final case object Y extends CyAdt

  final case class Z(c: CyAdt)

  object bigCycle {

    sealed trait Adt1
    final case class C1(i: Int, a: Adt2) extends Adt1
    final case object D1 extends Adt1

    sealed trait Adt2
    final case class C2(s: String, a: Adt3) extends Adt2
    final case object D2 extends Adt2

    sealed trait Adt3
    final case class C3(i: Int, a: Adt4) extends Adt3
    final case object D3 extends Adt3

    sealed trait Adt4
    final case class C4(a2: Adt2, a1: Adt1) extends Adt4
    final case class D4(a3: Adt3, b: String) extends Adt4
  }

  object smallCycle {
    final case class C(c: C)
    final case class X(x: C)
  }
}

class ModelCodecSpec extends BaseJsonSpec {

  import ModelCodecSpec._

  def atom[A](implicit atc: Atomic[A]): Json = {
    Json.obj(
      "Atom" -> Json.obj(
        "id" -> Json.fromString(atc.uuid.toString),
        "desc" -> Json.fromString(atc.description)
      )
    )
  }

  def sumRef(id: Int): Json =
    mkRef(id, "SumRef")

  def prodRef(id: Int): Json =
    mkRef(id, "ProdRef")

  def otherRef(id: Int): Json =
    mkRef(id, "OtherRef")

  def mkRef(id: Int, name: String): Json = {
    Json.obj(
      name -> Json.obj(
        "id" -> Json.fromString(id.toString)
      )
    )
  }

  val jfalse = Json.fromString(false.toString)
  val jtrue = Json.fromString(true.toString)
  val jNone = Json.obj("None" -> Json.obj())

  val a1 = Reified[Int].model
  val a2 = Reified[String].model
  val m1 = Reified[C].model
  val m1r = Reified[C].refined(C.refinement).model
  val m2 = Reified[Adt].model
  val cy1 = Reified[CyAdt].model
  val cy2 = Reified[Z].model
  val cy3 = Reified[bigCycle.Adt1].model
  val cy4 = Reified[smallCycle.C].model
  val cy5 = Reified[smallCycle.X].model

  // 'i -> Int :: 's -> String :: HNil
  val m1json = Json.obj(
    "HCons" -> Json.obj(
      "id" -> Json.fromString("1"),
      "label" -> Json.fromString("i"),
      "optional" -> jfalse,
      "refinement" -> jNone,
      "head" -> atom[Int],
      "tail" -> Json.obj(
        "HCons" -> Json.obj(
          "id" -> Json.fromString("0"),
          "label" -> Json.fromString("s"),
          "optional" -> jfalse,
          "refinement" -> jNone,
          "head" -> atom[String],
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          )
        )
      )
    )
  )

  val m1jsonShuffledKeys = Json.obj(
    "HCons" -> Json.obj(
      "refinement" -> jNone,
      "tail" -> Json.obj(
        "HCons" -> Json.obj(
          "head" -> atom[String],
          "refinement" -> jNone,
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          ),
          "optional" -> jfalse,
          "label" -> Json.fromString("s"),
          "id" -> Json.fromString("0")
        )
      ),
      "head" -> atom[Int],
      "label" -> Json.fromString("i"),
      "id" -> Json.fromString("1"),
      "optional" -> jfalse
    )
  )

  // α('i -> Int :: 's -> String :: HNil)ω
  val m1rJson = Json.obj(
    "HCons" -> Json.obj(
      "id" -> Json.fromString("1"),
      "label" -> Json.fromString("i"),
      "optional" -> jfalse,
      "refinement" -> Json.obj(
        "Some" -> Json.obj(
          "value" -> Json.obj(
            "uuid" -> Json.fromString(C.refinement.uuid.toString),
            "repr" -> Json.obj(
              "pre" -> Json.fromString("α"),
              "mid" -> Json.fromString("true"),
              "post" -> Json.fromString("ω")
            )
          )
        )
      ),
      "head" -> atom[Int],
      "tail" -> Json.obj(
        "HCons" -> Json.obj(
          "id" -> Json.fromString("0"),
          "label" -> Json.fromString("s"),
          "optional" -> jfalse,
          "refinement" -> jNone,
          "head" -> atom[String],
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          )
        )
      )
    )
  )

  // 'C -> ('i -> Int :: 's -> String :: HNil) :+: 'D -> HNil :+: CNil
  val m2json = Json.obj(
    "CCons" -> Json.obj(
      "id" -> Json.fromString("3"),
      "label" -> Json.fromString("C"),
      "refinement" -> jNone,
      "head" -> m1json,
      "tail" -> Json.obj(
        "CCons" -> Json.obj(
          "id" -> Json.fromString("2"),
          "label" -> Json.fromString("D"),
          "refinement" -> jNone,
          "head" -> Json.obj(
            "HNil" -> Json.obj()
          ),
          "tail" -> Json.obj(
            "CNil" -> Json.obj()
          )
        )
      )
    )
  )

  val cy1json = Json.obj(
    "CCons" -> Json.obj(
      "id" -> Json.fromString("2"),
      "label" -> Json.fromString("X"),
      "refinement" -> jNone,
      "head" -> Json.obj(
        "HCons" -> Json.obj(
          "id" -> Json.fromString("0"),
          "label" -> Json.fromString("m"),
          "optional" -> jfalse,
          "refinement" -> jNone,
          "head" -> sumRef(2),
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          )
        )
      ),
      "tail" -> Json.obj(
        "CCons" -> Json.obj(
          "id" -> Json.fromString("1"),
          "label" -> Json.fromString("Y"),
          "refinement" -> jNone,
          "head" -> Json.obj(
            "HNil" -> Json.obj()
          ),
          "tail" -> Json.obj(
            "CNil" -> Json.obj()
          )
        )
      )
    )
  )

  val cy2json = Json.obj(
    "HCons" -> Json.obj(
      "id" -> Json.fromString("3"),
      "label" -> Json.fromString("c"),
      "optional" -> jfalse,
      "refinement" -> jNone,
      "head" -> cy1json,
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  val optMod: Model = Model.HCons(
    'x,
    optional = true,
    Model.Atom.atom[Int],
    Model.HNil
  )

  val optMod2: Model = Model.HCons(
    'x,
    optional = false,
    Model.Atom.atom[Int],
    Model.HNil
  )

  val optJson = Json.obj(
    "HCons" -> Json.obj(
      "id" -> Json.fromString("0"),
      "label" -> Json.fromString("x"),
      "optional" -> jtrue,
      "refinement" -> jNone,
      "head" -> atom[Int],
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  val optJsonOmitted = Json.obj(
    "HCons" -> Json.obj(
      "id" -> Json.fromString("0"),
      "label" -> Json.fromString("x"),
      // no "optional" field
      // no "refinement" field
      "head" -> atom[Int],
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  "Encoding" - {

    "atoms" in {
      a1.asJson.spaces2 should === (atom[Int].spaces2)
      a2.asJson.spaces2 should === (atom[String].spaces2)
    }

    "simple models" in {
      m1.asJson.spaces2 should === (m1json.spaces2)
      m2.asJson.spaces2 should === (m2json.spaces2)
    }

    "cyclic models" in {
      cy1.asJson.spaces2 should === (cy1json.spaces2)
      cy2.asJson.spaces2 should === (cy2json.spaces2)
    }

    "optional fields" in {
      optMod.asJson.spaces2 should === (optJson.spaces2)
    }

    "refined models" in {
      m1r.asJson.spaces2 should === (m1rJson.spaces2)
    }
  }

  "Decoding" - {

    "atoms" in {
      atom[Int].as[Model] should === (
        Either.right(Model.Atom.atom[Int])
      )
      atom[String].as[Model] should === (
        Either.right(Model.Atom.atom[String])
      )
    }

    "simple models" in {
      m1json.as[Model] should === (Either.right(m1))
      m2json.as[Model] should === (Either.right(m2))
    }

    "cyclic models" in {
      cy1json.as[Model] should === (Either.right(cy1))
      cy2json.as[Model] should === (Either.right(cy2))
    }

    "order of keys shouldn't matter" in {
      m1jsonShuffledKeys.as[Model] should === (Either.right(m1))
    }

    "optional fields" in {
      optJson.as[Model] should === (Either.right(optMod))
      optJsonOmitted.as[Model] should === (Either.right(optMod2))
    }

    "refined models" in {
      m1rJson.as[Model] should === (Either.right(m1r))
    }

    "invalid data" - {

      // TODO: check error messages and cursor histories

      "bad structure" - {

        val good = Json.obj(
          "HCons" -> Json.obj(
            "id" -> Json.fromString("1"),
            "label" -> Json.fromString("i"),
            "optional" -> jfalse,
            "refinement" -> jNone,
            "head" -> atom[Int],
            "tail" -> Json.obj(
              "HCons" -> Json.obj(
                "id" -> Json.fromString("0"),
                "label" -> Json.fromString("s"),
                "optional" -> jfalse,
                "refinement" -> jNone,
                "head" -> atom[String],
                "tail" -> Json.obj(
                  "HNil" -> Json.obj()
                )
              )
            )
          )
        )

        "control" in {
          good.as[Model] shouldBe a [Right[_, _]]
        }

        "missing key" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("1"),
              "label" -> Json.fromString("i"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> atom[Int]
              // no "tail"
            )
          )
          bad.as[Model] should failWith(".*missing key: 'tail'.*")
        }

        "bad type of value" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("1"),
              "label" -> Json.fromString("i"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> atom[Int],
              "tail" -> Json.arr() // not an object
            )
          )
          bad.as[Model] should failWith(".*no variant matched.*")

          val bad2 = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("i"),
              "optional" -> Json.Null, // not a string
              "refinement" -> jNone,
              "head" -> atom[Int],
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )
          bad2.as[Model] should failWith("String")
        }
      }

      "bad reference" - {

        val good = Json.obj(
          "HCons" -> Json.obj(
            "id" -> Json.fromString("0"),
            "label" -> Json.fromString("c"),
            "optional" -> jfalse,
            "refinement" -> jNone,
            "head" -> Json.obj(
              "ProdRef" -> Json.obj(
                "id" -> Json.fromString("0")
              )
            ),
            "tail" -> Json.obj(
              "HNil" -> Json.obj()
            )
          )
        )

        "control" in {
          good.as[Model] shouldBe a [Right[_, _]]
        }

        "not a string" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> Json.obj(
                "ProdRef" -> Json.obj(
                  "id" -> Json.fromInt(56) // not string
                )
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )

          bad.as[Model] should failWith("String")
        }

        "not an Int.toString" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> Json.obj(
                "ProdRef" -> Json.obj(
                  "id" -> Json.fromString("abc") // NumberFormatException
                )
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )

          bad.as[Model] should failWith(".*error while decoding atom.*NumberFormatException.*")
        }

        "points to nonexistent ID" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> Json.obj(
                "ProdRef" -> Json.obj(
                  "id" -> Json.fromString("999") // no target
                )
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )

          bad.as[Model] should failWith(".*invalid ID.*")
        }

        "points to negative ID" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> Json.obj(
                "ProdRef" -> Json.obj(
                  "id" -> Json.fromString("-1") // no target
                )
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )

          bad.as[Model] should failWith(".*invalid ID.*")
        }

        "pointer from a tail" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> atom[String],
              "tail" -> Json.obj(
                "Ref" -> Json.obj(
                  "id" -> Json.fromString("0") // from tail
                )
              )
            )
          )

          bad.as[Model] should failWith(".*no variant matched.*")
        }

        "forward reference" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("1"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "refinement" -> jNone,
              "head" -> Json.obj(
                "ProdRef" -> Json.obj(
                  "id" -> Json.fromString("0") // forward ref
                )
              ),
              "tail" -> Json.obj(
                "HCons" -> Json.obj(
                  "id" -> Json.fromString("0"),
                  "label" -> Json.fromString("d"),
                  "optional" -> jfalse,
                  "refinement" -> jNone,
                  "head" -> atom[String],
                  "tail" -> Json.obj(
                    "HNil" -> Json.obj()
                  )
                )
              )
            )
          )

          bad.as[Model] should failWith(".*invalid ID.*")
        }
      }
    }
  }

  "Roundtrip" - {

    "built-in atoms" in {
      checkJson[Model](Model.Atom.atom[Int])
      checkJson[Model](Model.Atom.atom[String])
    }

    "custom atoms" in {
      import TestInstances.atomic.atomicMyUUID
      checkJson[Model](Model.Atom.atom[MyUUID])
    }

    "simple models" in {
      checkJson(m1)
      checkJson(m2)
    }

    "cyclic models" in {
      checkJson(cy1)
      checkJson(cy2)
      checkJson(cy3)
      checkJson(cy4)
      checkJson(cy5)
      checkJson(Reified[TestTypes.adts.recursive.IntList].model)
    }
  }
}


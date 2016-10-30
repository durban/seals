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

import java.util.UUID

import cats.data.Xor

import io.circe._
import io.circe.syntax._

import laws.MyUUID
import laws.TestInstances
import laws.TestTypes

import Codec._

object ModelCodecSpec {

  sealed trait Adt
  final case class C(i: Int, s: String) extends Adt
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

  def atom(id: UUID): Json = {
    Json.obj(
      "Atom" -> Json.obj(
        "id" -> Json.fromString(id.toString)
      )
    )
  }

  def ref(id: Int): Json = {
    Json.obj(
      "Ref" -> Json.obj(
        "id" -> Json.fromString(id.toString)
      )
    )
  }

  val jfalse = Json.fromString(false.toString)
  val jtrue = Json.fromString(true.toString)

  val a1 = Reified[Int].model
  val a2 = Reified[String].model
  val m1 = Reified[C].model
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
      "head" -> atom(Atom.builtinAtom[Int].uuid),
      "tail" -> Json.obj(
        "HCons" -> Json.obj(
          "id" -> Json.fromString("0"),
          "label" -> Json.fromString("s"),
          "optional" -> jfalse,
          "head" -> atom(Atom.builtinAtom[String].uuid),
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          )
        )
      )
    )
  )

  val m1jsonShuffledKeys = Json.obj(
    "HCons" -> Json.obj(
      "tail" -> Json.obj(
        "HCons" -> Json.obj(
          "head" -> atom(Atom.builtinAtom[String].uuid),
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          ),
          "optional" -> jfalse,
          "label" -> Json.fromString("s"),
          "id" -> Json.fromString("0")
        )
      ),
      "head" -> atom(Atom.builtinAtom[Int].uuid),
      "label" -> Json.fromString("i"),
      "id" -> Json.fromString("1"),
      "optional" -> jfalse
    )
  )

  // 'C -> ('i -> Int :: 's -> String :: HNil) :+: 'D -> HNil :+: CNil
  val m2json = Json.obj(
    "CCons" -> Json.obj(
      "id" -> Json.fromString("3"),
      "label" -> Json.fromString("C"),
      "head" -> m1json,
      "tail" -> Json.obj(
        "CCons" -> Json.obj(
          "id" -> Json.fromString("2"),
          "label" -> Json.fromString("D"),
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
      "head" -> Json.obj(
        "HCons" -> Json.obj(
          "id" -> Json.fromString("0"),
          "label" -> Json.fromString("m"),
          "optional" -> jfalse,
          "head" -> ref(2),
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          )
        )
      ),
      "tail" -> Json.obj(
        "CCons" -> Json.obj(
          "id" -> Json.fromString("1"),
          "label" -> Json.fromString("Y"),
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
      "head" -> cy1json,
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  val optMod: Model = Model.HCons(
    'x,
    optional = true,
    Atom.builtinAtom[Int],
    Model.HNil
  )

  val optMod2: Model = Model.HCons(
    'x,
    optional = false,
    Atom.builtinAtom[Int],
    Model.HNil
  )

  val optJson = Json.obj(
    "HCons" -> Json.obj(
      "id" -> Json.fromString("0"),
      "label" -> Json.fromString("x"),
      "optional" -> jtrue,
      "head" -> atom(Atom.builtinAtom[Int].uuid),
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
      "head" -> atom(Atom.builtinAtom[Int].uuid),
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  "Encoding" - {

    "atoms" in {
      a1.asJson should === (atom(Atom.builtinAtom[Int].uuid))
      a2.asJson should === (atom(Atom.builtinAtom[String].uuid))
    }

    "simple models" in {
      m1.asJson should === (m1json)
      m2.asJson should === (m2json)
    }

    "cyclic models" in {
      cy1.asJson should === (cy1json)
      cy2.asJson should === (cy2json)
    }

    "optional fields" in {
      optMod.asJson should === (optJson)
    }
  }

  "Decoding" - {

    "atoms" in {
      atom(Atom.builtinAtom[Int].uuid).as[Model] should === (
        Xor.right(Atom[Int])
      )
      atom(Atom.builtinAtom[String].uuid).as[Model] should === (
        Xor.right(Atom[String])
      )
    }

    "simple models" in {
      m1json.as[Model] should === (Xor.right(m1))
      m2json.as[Model] should === (Xor.right(m2))
    }

    "cyclic models" in {
      cy1json.as[Model] should === (Xor.right(cy1))
      cy2json.as[Model] should === (Xor.right(cy2))
    }

    "order of keys shouldn't matter" in {
      m1jsonShuffledKeys.as[Model] should === (Xor.right(m1))
    }

    "optional fields" in {
      optJson.as[Model] should === (Xor.right(optMod))
      optJsonOmitted.as[Model] should === (Xor.right(optMod2))
    }

    "invalid data" - {

      // TODO: check error messages and cursor histories

      "bad structure" - {

        val good = Json.obj(
          "HCons" -> Json.obj(
            "id" -> Json.fromString("1"),
            "label" -> Json.fromString("i"),
            "optional" -> jfalse,
            "head" -> atom(Atom.builtinAtom[Int].uuid),
            "tail" -> Json.obj(
              "HCons" -> Json.obj(
                "id" -> Json.fromString("0"),
                "label" -> Json.fromString("s"),
                "optional" -> jfalse,
                "head" -> atom(Atom.builtinAtom[String].uuid),
                "tail" -> Json.obj(
                  "HNil" -> Json.obj()
                )
              )
            )
          )
        )

        "control" in {
          good.as[Model] shouldBe a [Xor.Right[_]]
        }

        "missing key" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("1"),
              "label" -> Json.fromString("i"),
              "optional" -> jfalse,
              "head" -> atom(Atom.builtinAtom[Int].uuid)
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
              "head" -> atom(Atom.builtinAtom[Int].uuid),
              "tail" -> Json.arr() // not an object
            )
          )
          bad.as[Model] should failWith(".*no variant matched.*")

          val bad2 = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("i"),
              "optional" -> Json.Null, // not a string
              "head" -> atom(Atom.builtinAtom[Int].uuid),
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
            "head" -> Json.obj(
              "Ref" -> Json.obj(
                "id" -> Json.fromString("0")
              )
            ),
            "tail" -> Json.obj(
              "HNil" -> Json.obj()
            )
          )
        )

        "control" in {
          good.as[Model] shouldBe a [Xor.Right[_]]
        }

        "not a string" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "head" -> Json.obj(
                "Ref" -> Json.obj(
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
              "head" -> Json.obj(
                "Ref" -> Json.obj(
                  "id" -> Json.fromString("abc") // not an Int.toString
                )
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )

          bad.as[Model] should failWith(".*cannot decode atom.*")
        }

        "points to nonexistent ID" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "id" -> Json.fromString("0"),
              "label" -> Json.fromString("c"),
              "optional" -> jfalse,
              "head" -> Json.obj(
                "Ref" -> Json.obj(
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
              "head" -> Json.obj(
                "Ref" -> Json.obj(
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
              "head" -> atom(Atom.builtinAtom[String].uuid),
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
              "head" -> Json.obj(
                "Ref" -> Json.obj(
                  "id" -> Json.fromString("0") // forward ref
                )
              ),
              "tail" -> Json.obj(
                "HCons" -> Json.obj(
                  "id" -> Json.fromString("0"),
                  "label" -> Json.fromString("d"),
                  "optional" -> jfalse,
                  "head" -> atom(Atom.builtinAtom[String].uuid),
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
      checkJson[Model](Atom[Int])
      checkJson[Model](Atom[String])
    }

    "custom atoms" in {
      // TODO: better API for custom registries
      import TestInstances.atomic.atomicMyUUID
      implicit val r: AtomRegistry = AtomRegistry.builtinAtomRegistry + Atom[MyUUID]
      checkJson[Model](Atom[MyUUID])
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


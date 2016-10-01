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

import ModelCodec._

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

  def ref(uri: String): Json = {
    JsonRef(uri).map(_.asJson).fold(
      msg => fail(s"invalid ref: $uri ($msg)"),
      identity
    )
  }

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
      "label" -> Json.fromString("i"),
      "optional" -> Json.False,
      "head" -> atom(Atom.builtinAtom[Int].uuid),
      "tail" -> Json.obj(
        "HCons" -> Json.obj(
          "label" -> Json.fromString("s"),
          "optional" -> Json.False,
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
          "optional" -> Json.False,
          "label" -> Json.fromString("s")
        )
      ),
      "head" -> atom(Atom.builtinAtom[Int].uuid),
      "label" -> Json.fromString("i"),
      "optional" -> Json.False
    )
  )

  // 'C -> ('i -> Int :: 's -> String :: HNil) :+: 'D -> HNil :+: CNil
  val m2json = Json.obj(
    "CCons" -> Json.obj(
      "label" -> Json.fromString("C"),
      "head" -> m1json,
      "tail" -> Json.obj(
        "CCons" -> Json.obj(
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

  val cy1json = cy1(ref("#"))

  val cy2json = Json.obj(
    "HCons" -> Json.obj(
      "label" -> Json.fromString("c"),
      "optional" -> Json.False,
      "head" -> cy1(ref("#/HCons/head")),
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  def cy1(ref: Json): Json = Json.obj(
    "CCons" -> Json.obj(
      "label" -> Json.fromString("X"),
      "head" -> Json.obj(
        "HCons" -> Json.obj(
          "label" -> Json.fromString("m"),
          "optional" -> Json.False,
          "head" -> ref,
          "tail" -> Json.obj(
            "HNil" -> Json.obj()
          )
        )
      ),
      "tail" -> Json.obj(
        "CCons" -> Json.obj(
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
      "label" -> Json.fromString("x"),
      "optional" -> Json.True,
      "head" -> atom(Atom.builtinAtom[Int].uuid),
      "tail" -> Json.obj(
        "HNil" -> Json.obj()
      )
    )
  )

  val optJsonOmitted = Json.obj(
    "HCons" -> Json.obj(
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
            "label" -> Json.fromString("i"),
            "optional" -> Json.False,
            "head" -> atom(Atom.builtinAtom[Int].uuid),
            "tail" -> Json.obj(
              "HCons" -> Json.obj(
                "label" -> Json.fromString("s"),
                "optional" -> Json.False,
                "head" -> atom(Atom.builtinAtom[String].uuid),
                "tail" -> Json.obj(
                  "HNil" -> Json.obj()
                )
              )
            )
          )
        )

        "missing key" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("i"),
              "optional" -> Json.False,
              "head" -> atom(Atom.builtinAtom[Int].uuid)
              // no "tail"
            )
          )
          bad.as[Model] should failWith(".*failed cursor.*")
        }

        "bad type of value" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("i"),
              "optional" -> Json.False,
              "head" -> atom(Atom.builtinAtom[Int].uuid),
              "tail" -> Json.arr() // not an object
            )
          )
          bad.as[Model] should failWith("JsonObject")

          val bad2 = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("i"),
              "optional" -> Json.Null, // not a bool
              "head" -> atom(Atom.builtinAtom[Int].uuid),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )
          bad2.as[Model] should failWith("Boolean")
        }
      }

      "bad reference" - {

        val good = Json.obj(
          "HCons" -> Json.obj(
            "label" -> Json.fromString("c"),
            "optional" -> Json.False,
            "head" -> Json.obj(
              JsonRef.key -> Json.fromString("#")
            ),
            "tail" -> Json.obj(
              "HNil" -> Json.obj()
            )
          )
        )

        "not a string" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromInt(56) // not string
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )
          bad.as[Model] should failWith("String")
        }

        "not an URI" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromString("a_b_c://boo") // not URI
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )
          bad.as[Model] should failWith(".*Illegal character.*")
        }

        "points to nowhere" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromString("#/HCons/foo/bar") // no target
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )
          bad.as[Model] should failWith(".*invalid backreference.*")
        }

        "points to outside the JSON" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromString("http://www.example.com#/HCons/head") // external
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              )
            )
          )
          bad.as[Model] should failWith(".*nonempty host.*")
        }

        "points into an array" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromString("#/HCons/foobar/0") // into array
              ),
              "tail" -> Json.obj(
                "HNil" -> Json.obj()
              ),
              "foobar" -> Json.arr(
                Json.obj(
                  "HNil" -> Json.obj()
                )
              )
            )
          )
          bad.as[Model] should failWith(".*invalid backreference.*")
        }

        "points to an Atom" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> atom(Atom.builtinAtom[String].uuid),
              "tail" -> Json.obj(
                "HCons" -> Json.obj(
                  "label" -> Json.fromString("x"),
                  "optional" -> Json.False,
                  "head" -> Json.obj(
                    JsonRef.key -> Json.fromString("#/HCons/head") // to atom
                  ),
                  "tail" -> Json.obj(
                    "HNil" -> Json.obj()
                  )
                )
              )
            )
          )
          bad.as[Model] should failWith(".*invalid backreference.*")
        }

        "pointer from a tail" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("c"),
              "optional" -> Json.False,
              "head" -> atom(Atom.builtinAtom[String].uuid),
              "tail" -> Json.obj(
                JsonRef.key -> Json.fromString("#") // from tail
              )
            )
          )
          bad.as[Model] should failWith(".*not a HList.*")
        }

        "points to a ref" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("i"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromString("#")
              ),
              "tail" -> Json.obj(
                "HCons" -> Json.obj(
                  "label" -> Json.fromString("s"),
                  "optional" -> Json.False,
                  "head" -> Json.obj(
                    JsonRef.key -> Json.fromString("#/HCons/head") // to another ref
                  ),
                  "tail" -> Json.obj(
                    "HNil" -> Json.obj()
                  )
                )
              )
            )
          )
          bad.as[Model] should failWith(".*invalid backreference.*")
        }

        "forward reference" in {
          val bad = Json.obj(
            "HCons" -> Json.obj(
              "label" -> Json.fromString("i"),
              "optional" -> Json.False,
              "head" -> Json.obj(
                JsonRef.key -> Json.fromString("#/HCons/tail") // forward
              ),
              "tail" -> Json.obj(
                "HCons" -> Json.obj(
                  "label" -> Json.fromString("s"),
                  "optional" -> Json.False,
                  "head" -> atom(Atom.builtinAtom[String].uuid),
                  "tail" -> Json.obj(
                    "HNil" -> Json.obj()
                  )
                )
              )
            )
          )
          bad.as[Model] should failWith(".*invalid backreference.*")
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


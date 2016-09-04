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
package tests

import shapeless.test.illTyped

import laws.TestTypes

// TODO: test runtime compat for all of these

class CompatSpec extends BaseSpec {

  "Compat[O, N] exists" - {

    "isomorphic" - {
      import TestTypes.adts.iso._

      "case classes" in {
        Compat[Adt1.Foo, Adt2.Foo]
        Compat[Adt2.Foo, Adt1.Foo]
      }

      "ADTs" in {
        Compat[Adt1, Adt2]
        Compat[Adt2, Adt1]
      }
    }

    "when field(s) with default value(s)" - {
      import TestTypes.adts.defs._

      "one added" in {
        Compat[Adt1.C, Adt2.C]
        Compat[Adt3.C, Adt1.C]
        Compat[Adt1, Adt2]
        Compat[Adt3, Adt1]
      }

      "one removed" in {
        Compat[Adt2.C, Adt1.C]
        Compat[Adt1.C, Adt3.C]
        Compat[Adt2, Adt1]
        Compat[Adt1, Adt3]
      }

      "two added" in {
        Compat[Adt3.C, Adt2.C]
        Compat[Adt3, Adt2]
      }

      "two removed" in {
        Compat[Adt2.C, Adt3.C]
        Compat[Adt2, Adt3]
      }
    }

    "new field with default + one compatible field type change" in {
      import TestTypes.adts.defsComp._
      Compat[Person1, Person2]
      Compat[Person2, Person1]
      Compat[C1, C2]
      Compat[C2, C1]
    }

    "new field with default + one compatible field type change (also default)" in {
      import TestTypes.adts.defsCompDefs._
      Compat[Person1, Person2]
      Compat[Person2, Person1]
      Compat[C1, C2]
      Compat[C2, C1]
    }
  }

  "Negative tests" - {

    "when field(s) without default value(s)" - {
      import TestTypes.adts.nodefs._

      "directly" in {
        illTyped("Compat[Adt1.C, Adt2.C]", notFound)
        illTyped("Compat[Adt1.C, Adt3.C]", notFound)
        illTyped("Compat[Adt2.C, Adt1.C]", notFound)
        illTyped("Compat[Adt2.C, Adt3.C]", notFound)
        illTyped("Compat[Adt3.C, Adt1.C]", notFound)
        illTyped("Compat[Adt3.C, Adt2.C]", notFound)
      }

      "deep in an ADT" in {
        illTyped("Compat[Adt1, Adt2]", notFound)
        illTyped("Compat[Adt1, Adt3]", notFound)
        illTyped("Compat[Adt2, Adt1]", notFound)
        illTyped("Compat[Adt2, Adt3]", notFound)
        illTyped("Compat[Adt3, Adt1]", notFound)
        illTyped("Compat[Adt3, Adt2]", notFound)
      }
    }

    "when fields are renamed" in {
      import TestTypes.adts.rename._
      illTyped("Compat[C1, C2]", notFound)
      illTyped("Compat[C2, C1]", notFound)
    }

    "when ADT leafs are renamed" in {
      import TestTypes.adts.rename._
      illTyped("Compat[v1.Adt, v2.Adt]", notFound)
      illTyped("Compat[v2.Adt, v1.Adt]", notFound)
    }
  }
}

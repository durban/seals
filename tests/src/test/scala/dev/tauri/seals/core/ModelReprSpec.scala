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
package core

import cats.implicits._

import org.scalatest.compatible.Assertion

class ModelReprSpec extends tests.BaseSpec {

  def roundtrip(m: Model): Assertion = {
    val r = ModelRepr.fromModel(m)
    val m2 = r.toModel.fold(err => fail(sh"cannot decode ModelRepr: ${err}"), m => m)
    assert(Model.modelEquality.eqv(m2, m))
    val r2 = ModelRepr.fromModel(m2)
    assert(r2 === r)
  }

  val modelModel = Reified[Model].model

  "Cycles/Refs" in {
    val modelUnderTest = Model.Vector(
      Model.HCons(
        '_1,
        Atomic[Int].atom,
        Model.HCons(
          '_2,
          modelModel,
          Model.HNil
        )
      )
    )

    roundtrip(modelModel)
    roundtrip(Model.Vector(modelModel))
    roundtrip(modelUnderTest)
  }

  "Long HCons" in {
    val modelUnderTest = Model.Vector(
      'a -> modelModel ::
      'b -> modelModel ::
      'c -> modelModel ::
      'd -> modelModel ::
      'e -> modelModel ::
      'f -> modelModel ::
      'g -> modelModel ::
      'h -> modelModel ::
      Model.HNil
    )

    roundtrip(modelUnderTest)
  }
}

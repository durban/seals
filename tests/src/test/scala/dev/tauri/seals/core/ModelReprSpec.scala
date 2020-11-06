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
    val r = reprFromModel(m)
    val m2 = modelFromRepr(r)
    assert(Model.modelEquality.eqv(m2, m))
    val r2 = reprFromModel(m2)
    assert(ModelRepr.eqInstance.eqv(r2, r))
  }

  private def reprFromModel(m: Model): ModelRepr = {
    ModelRepr.fromModel(m)
  }

  private def modelFromRepr(r: ModelRepr): Model = {
    r.toModel.fold(err => fail(sh"cannot decode ModelRepr: ${err}"), m => m)
  }

  private val modelModel = Reified[Model].model
  private val modelModelRepr = reprFromModel(modelModel)

  "Cycles/Refs" in {
    val modelUnderTest = Model.Vector(
      Model.HCons(
        Symbol("_1"),
        Atomic[Int].atom,
        Model.HCons(
          Symbol("_2"),
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
      Symbol("a") -> modelModel ::
      Symbol("b") -> modelModel ::
      Symbol("c") -> modelModel ::
      Symbol("d") -> modelModel ::
      Symbol("e") -> modelModel ::
      Symbol("f") -> modelModel ::
      Symbol("g") -> modelModel ::
      Symbol("h") -> modelModel ::
      Model.HNil
    )

    roundtrip(modelUnderTest)
  }

  final val N = 30000

  "Encoding performance" in {
    (1 to N).map { _ =>
      reprFromModel(modelModel)
    }
  }

  "Decoding performance" in {
    (1 to N).map { _ =>
      modelFromRepr(modelModelRepr)
    }
  }
}

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
package extractor

import scala.reflect.runtime.{ universe => ru }

import org.scalatest.{ FlatSpec, Matchers }

import io.circe.{ Decoder, JsonObject }

import io.sigs.seals.circe.Codecs._

class MainSpec extends FlatSpec with Matchers {

  val decoder = Decoder[Model]
  val pack = this.getClass.getPackage.getName

  val fooName = ru.symbolOf[Foo].fullName
  val ccName = ru.symbolOf[CC].fullName
  val wfooName = ru.symbolOf[Wrap.WFoo].fullName
  val wccName = ru.symbolOf[Wrap.WCC].fullName

  // force WFoo subclasses (SI-7046 workaround):
  def dummy1: Wrap.Bar = ???
  def dummy2: Wrap.Baz.type = ???

  val main = Main(this.getClass.getClassLoader)

  "Main.extractAll" should "find all marked classes in a package" in {
    val json = main.extractAll(pack)
    val models = json.as[JsonObject]
      .fold(err => fail(err.toString), identity)
      .toMap
      .mapValues(j => decoder.decodeJson(j).fold(err => fail(err.toString), identity))
    val expected = Map(
      fooName -> Foo.$io$sigs$seals$core$Reified$Instance.model,
      ccName -> CC.$io$sigs$seals$core$Reified$Instance.model,
      wfooName -> Wrap.WFoo.$io$sigs$seals$core$Reified$Instance.model,
      wccName -> Wrap.WCC.$io$sigs$seals$core$Reified$Instance.model
    )
    models should === (expected)
  }

  "Main.allSchemas" should "collect all annotated classes" in {
    main.allSchemas(pack).map(_.fullName).toSet should === (Set(
      fooName,
      ccName,
      wfooName,
      wccName
    ))
  }
}

/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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
package checker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import io.circe.{ Decoder, JsonObject }

import circe.Codecs._

class ExtractorSpec extends AnyFlatSpec with Matchers {

  val decoder = Decoder[Model]
  val pack = this.getClass.getPackage.getName

  val fooName = s"$pack.Foo"
  val ccName = s"$pack.CC"
  val wfooName = s"$pack.Wrap.WFoo"
  val wccName = s"$pack.Wrap.WCC"

  // force WFoo subclasses (SI-7046 workaround):
  def dummy1: Wrap.Bar = sys.error("dummy1")
  def dummy2: Wrap.Baz.type = sys.error("dummy2")

  val extractor = Extractor(
    this.getClass.getClassLoader,
    new java.io.File(this.getClass.getProtectionDomain.getCodeSource.getLocation.toURI)
  )

  "Extractor#allClasses" should "find every class" in {
    extractor.allClasses(pack) should contain allOf (
      "Foo",
      "Foo$",
      "CC",
      "CC$",
      "Wrap$"
    )
  }

  "Extractor#extractAll" should "find all marked classes in a package" in {
    val json = extractor.extractAll(pack)
    val models = json.as[JsonObject]
      .fold(err => fail(err.toString), identity)
      .toMap
      .mapValues(j => decoder.decodeJson(j).fold(err => fail(err.toString), identity))
    val expected = Map(
      fooName -> Foo.reifiedFoo.model,
      ccName -> CC.reifiedCC.model,
      wfooName -> Wrap.WFoo.reifiedWFoo.model,
      wccName -> Wrap.WCC.reifiedWCC.model
    )
    models should === (expected)
  }

  "Extractor#allSchemas" should "collect all annotated classes" in {
    extractor.allSchemasOfPackage(pack).map(_.fullName).toSet should === (Set(
      fooName,
      ccName,
      wfooName,
      wccName
    ))
  }
}

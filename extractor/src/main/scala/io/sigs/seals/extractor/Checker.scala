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

import java.nio.file.{ Files, Paths }

import scala.collection.JavaConverters._
import scala.util.control.NoStackTrace

import io.circe.parser

import io.sigs.seals.circe.Codecs._
import java.nio.charset.StandardCharsets

object Checker {

  final class Error(msg: String)
    extends Throwable(msg)
    with NoStackTrace

  def error(msg: String): Nothing =
    throw new Error(msg)

  def main(args: Array[String]): Unit = {
    val Array(curr, prev) = args
    val currModels = loadModels(curr)
    val prevModels = loadModels(prev)

    compareModelSet(currModels, prevModels)

    // at this point we're OK:
    Console.println("All models are compatible.")
  }

  def loadModels(path: String): Map[String, Map[String, Model]] = {
    val str = Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8).asScala.mkString
    parser.decode[Map[String, Map[String, Model]]](str).fold(
      err => throw new IllegalArgumentException(s"error while decoding '${path}': ${err}"),
      models => models
    )
  }

  def compareModelSet(currModels: Map[String, Map[String, Model]], prevModels: Map[String, Map[String, Model]]): Unit = {
    assertSameKeys("packages", currModels, prevModels)
    for (pack <- currModels.keySet) {
      val curr = currModels(pack)
      val prev = prevModels(pack)
      comparePackage(curr, prev)
    }
  }

  def comparePackage(currModels: Map[String, Model], prevModels: Map[String, Model]): Unit = {
    assertSameKeys("schemata", currModels, prevModels)
    for (sch <- currModels.keySet) {
      val curr = currModels(sch)
      val prev = prevModels(sch)
      assertCompatible(sch, curr, prev)
    }
  }

  def assertCompatible(name: String, curr: Model, prev: Model): Unit = {
    val ok = curr compatible prev
    if (!ok) {
      error(mkList(
        s"Schema '${name}' changed in an incompatible way",
        s"current: ${curr}" :: s"previous: ${prev}" :: Nil
      ))
    }
  }

  private def assertSameKeys[A](label: String, curr: Map[String, A], prev: Map[String, A]): Unit = {
    val news = curr.keySet -- prev.keySet
    if (news.nonEmpty) {
      error(mkList(s"New ${label}: ", news))
    }
    val deleted = prev.keySet -- curr.keySet
    if (deleted.nonEmpty) {
      error(mkList(s"Deleted ${label}: ", deleted))
    }
  }

  private def mkList(
    header: String,
    els: Iterable[String],
    sort: Boolean = true,
    indent: String = "  "
  ): String = {
    val v = if (sort) els.toVector.sorted else els.toVector
    (header +: v.map(indent + _)).mkString("\n")
  }
}

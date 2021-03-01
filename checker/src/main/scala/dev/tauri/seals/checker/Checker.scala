/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package checker

import java.nio.file.{ Files, Paths }
import java.nio.charset.StandardCharsets

import scala.jdk.CollectionConverters._
import scala.util.control.NoStackTrace

import cats.implicits._

import io.circe.parser

object Checker {

  type ErrorMsg = String
  type Result = Option[ErrorMsg]
  type Report = Map[String, Map[String, Result]]

  final class Error(msg: String)
    extends Throwable(msg)
    with NoStackTrace

  def error(msg: String): Nothing =
    throw new Error(msg)

  def main(args: Array[String]): Unit = {
    val (curr, prev) = args match {
      case Array(c, p) => (c, p)
      case _ => throw new IllegalArgumentException(args.mkString(", "))
    }
    val currModels = loadModels(curr)
    val prevModels = loadModels(prev)

    val r: Report = compareModelSet(currModels, prevModels)
    assertOk(r)

    // at this point we're OK:
    val nModels = r.values.map(_.size).sum
    Console.println(sh"All ${nModels} checked models are compatible.") // scalastyle:ignore regex
  }

  def loadModels(path: String): ModelSet = {
    val str = Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8).asScala.mkString
    parser.decode[ModelSet](str).fold(
      err => throw new IllegalArgumentException(sh"error while decoding '${path}': ${err}"),
      models => models
    )
  }

  def compareModelSet(curr: ModelSet, prev: ModelSet): Report = {
    assertSameKeys("packages", curr.models, prev.models)
    (for (pack <- curr.models.keys) yield {
      val currPak = curr.models(pack)
      val prevPak = prev.models(pack)
      pack -> comparePackage(currPak, prevPak)
    }).toMap
  }

  def comparePackage(currModels: Map[String, Model], prevModels: Map[String, Model]): Map[String, Result] = {
    assertSameKeys("schemata", currModels, prevModels)
    (for (sch <- currModels.keys) yield {
      val curr = currModels(sch)
      val prev = prevModels(sch)
      sch -> checkCompatible(sch, curr, prev)
    }).toMap
  }

  def checkCompatible(name: String, curr: Model, prev: Model): Result = {
    val ok = curr compatible prev
    if (!ok) {
      Some(mkList(
        sh"Schema '${name}' changed in an incompatible way",
        sh"current:  ${curr}" ::
        sh"previous: ${prev}" ::
        Nil
      ))
    } else {
      None
    }
  }

  private def assertSameKeys[A](label: String, curr: Map[String, A], prev: Map[String, A]): Unit = {
    val news = curr.keySet -- prev.keySet
    if (news.nonEmpty) {
      error(mkList(sh"New ${label}: ", news))
    }
    val deleted = prev.keySet -- curr.keySet
    if (deleted.nonEmpty) {
      error(mkList(sh"Deleted ${label}: ", deleted))
    }
  }

  private def assertOk(r: Report): Unit = {
    for ((pack, schs) <- r) {
      for ((sch, res) <- schs) {
        res.foreach { err =>
          error(err)
        }
      }
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

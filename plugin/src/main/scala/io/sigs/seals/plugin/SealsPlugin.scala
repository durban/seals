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
package plugin

import sbt._
import sbt.Keys._

object SealsPlugin extends AutoPlugin {

  override def requires = plugins.JvmPlugin

  override def trigger = allRequirements

  val autoImport: SealsKeys = new SealsKeys {}
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    sealsSchemaPackages := Nil,
    sealsSchemaTarget := crossTarget.value / "seals",
    sealsCheckSchema := checkTask.value,
    libraryDependencies += "io.sigs" %% "seals-extractor" % BuildInfo.version % "compile-internal"
  )

  lazy val checkTask = Def.task {
    val streams = Keys.streams.value
    val runner = (Keys.runner in Compile).value
    val classpath = (fullClasspath in Compile).value
    val classdir = (classDirectory in Compile).value
    check(
      streams,
      runner,
      classpath,
      classdir,
      sealsSchemaTarget.value,
      sealsSchemaPackages.value
    )
  }

  def check(
    streams: TaskStreams,
    runner: ScalaRun,
    classpath: Classpath,
    classdir: File,
    outdir: File,
    packs: Seq[String]
  ): Unit = {
    assert(outdir.mkdirs())
    val output = runner.run(
      mainClass = "io.sigs.seals.extractor.Extractor",
      classpath = sbt.Attributed.data(classpath),
      options = classdir.getAbsolutePath +: outdir.getAbsolutePath +: packs,
      log = streams.log
    )
    toError(output)
  }
}

sealed abstract class SealsKeys {

  final val sealsSchemaPackages = settingKey[Seq[String]]("packages")

  final val sealsSchemaTarget = settingKey[File]("target")

  final val sealsCheckSchema = taskKey[Unit]("check")
}

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

import com.typesafe.tools.mima

object SealsPlugin extends AutoPlugin { self =>

  override def requires = plugins.JvmPlugin && mima.plugin.MimaPlugin

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

    def check(classdir: File, target: File): Unit = {
      val streams = Keys.streams.value
      val runner = (Keys.runner in Compile).value
      // FIXME: This won't work if a previous artifact
      // FIXME: depends on an incompatible version of us.
      val classpath = (fullClasspath in Compile).value
      self.check(
        streams,
        runner,
        classpath,
        classdir,
        target,
        sealsSchemaPackages.value
      )
    }

    val targetDir = sealsSchemaTarget.value

    val current: File = mima.plugin.MimaKeys.mimaCurrentClassfiles.value
    check(current, targetDir / "current.json")

    val previous: Map[ModuleID, File] = mima.plugin.MimaKeys.mimaPreviousClassfiles.value
    for ((module, prev) <- previous) {
      check(prev, targetDir / "previous" / s"${module}.json")
    }
  }

  def check(
    streams: TaskStreams,
    runner: ScalaRun,
    classpath: Classpath,
    classdir: File,
    targetFile: File,
    packs: Seq[String]
  ): Unit = {
    assert(targetFile.getAbsoluteFile.getParentFile.mkdirs())
    val output = runner.run(
      mainClass = "io.sigs.seals.extractor.Extractor",
      classpath = sbt.Attributed.data(classpath),
      options = classdir.getAbsolutePath +: targetFile.getAbsolutePath +: packs,
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

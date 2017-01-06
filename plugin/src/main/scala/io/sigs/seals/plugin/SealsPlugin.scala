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

  final val namespace = "seals"

  def nsScalaVer(ver: String) = s"${namespace}_${ver}"

  override def requires = plugins.JvmPlugin && mima.plugin.MimaPlugin

  override def trigger = allRequirements

  val autoImport: SealsKeys = new SealsKeys {}
  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    sealsSchemaPackages := Nil,
    sealsSchemaTarget := crossTarget.value / namespace,
    sealsCheckSchema := checkTask.value,
    sealsExtractSchema := extractTask.value,
    libraryDependencies += "io.sigs" %% "seals-extractor" % BuildInfo.version % "compile-internal"
  )

  lazy val checkTask = Def.task[Unit] {
    val (curr, prevModels) = sealsExtractSchema.value
    for (prev <- prevModels.toVector.sorted) {
      checkCompat(
        streams.value,
        (runner in Compile).value,
        (fullClasspath in Compile).value,
        curr,
        prev
      )
    }
  }

  lazy val extractTask = Def.task[(File, Set[File])] {

    val streams = Keys.streams.value

    def extractOne(
      classdir: File,
      target: File,
      remove: Set[File] = Set.empty
    ) = {
      // FIXME: This won't work if a previous artifact
      // FIXME: depends on an incompatible version of us.
      val classpath = sbt.Attributed.data((fullClasspath in Compile).value)
      val modifiedClasspath = classdir +: classpath.filterNot(remove.contains)
      extract(
        streams,
        (runner in Compile).value,
        modifiedClasspath,
        classdir,
        target,
        sealsSchemaPackages.value
      )
    }

    val targetDir = sealsSchemaTarget.value
    val current: File = mima.plugin.MimaKeys.mimaCurrentClassfiles.value
    val currTarget = targetDir / "current.json"
    val previous: Map[ModuleID, File] = mima.plugin.MimaKeys.mimaPreviousClassfiles.value

    val extractAll: (Set[File] => Set[File]) = FileFunction.cached(
      streams.cacheDirectory / nsScalaVer(scalaBinaryVersion.value),
      inStyle = FilesInfo.hash,
      outStyle = FilesInfo.hash
    ) { in: Set[File] =>
      extractOne(current, currTarget)
      val prevs = previous.map { case (module, prev) =>
        val targetFile = targetDir / "previous" / s"${module}.json"
        extractOne(prev, targetFile, remove = Set(current))
        targetFile
      }.toSet
      prevs + currTarget
    }

    val in = (previous.values.toSet + current).flatMap(allFiles)
    streams.log.debug(("Input files:" +: in.toList.sorted.map("  " + _)).mkString("\n"))
    val out = extractAll(in)
    streams.log.debug(("Output files:" +: out.toList.sorted.map("  " + _)).mkString("\n"))
    assert(out contains currTarget)
    (currTarget, out - currTarget)
  }

  def extract(
    streams: TaskStreams,
    runner: ScalaRun,
    classpath: Seq[File],
    classdir: File,
    targetFile: File,
    packs: Seq[String]
  ): Unit = {
    streams.log.debug(s"Starting extractor with classpath: ${classpath}")
    targetFile.getAbsoluteFile.getParentFile.mkdirs()
    val res = runner.run(
      mainClass = "io.sigs.seals.extractor.Extractor",
      classpath = classpath,
      options = classdir.getAbsolutePath +: targetFile.getAbsolutePath +: packs,
      log = streams.log
    )
    toError(res)
    assert(targetFile.exists)
  }

  def checkCompat(
    streams: TaskStreams,
    runner: ScalaRun,
    classpath: Classpath,
    current: File,
    previous: File
  ): Unit = {
    assert(current.exists)
    assert(previous.exists)
    val res = runner.run(
      mainClass = "io.sigs.seals.extractor.Checker",
      classpath = sbt.Attributed.data(classpath),
      options = current.getAbsolutePath :: previous.getAbsolutePath :: Nil,
      log = streams.log
    )
    toError(res)
  }

  private def allFiles(f: File): Set[File] = {
    if (f.isFile) {
      Set(f)
    } else if (f.isDirectory) {
      f.listFiles().toSet.flatMap(allFiles)
    } else {
      Set.empty
    }
  }
}

sealed abstract class SealsKeys {

  final val sealsSchemaPackages = settingKey[Seq[String]]("packages")

  final val sealsSchemaTarget = settingKey[File]("target")

  final val sealsCheckSchema = taskKey[Unit]("check")

  final val sealsExtractSchema = taskKey[(File, Set[File])]("extract")
}

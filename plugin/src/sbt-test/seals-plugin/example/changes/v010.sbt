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

lazy val proto = project
  .settings(name := "example-proto")
  .settings(commonSettings)
  .settings(
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch),
    mimaPreviousArtifacts := Set(organization.value %% name.value % "0.1.0-SNAPSHOT"),
    sealsSchemaPackages += "com.example.proto"
  )

lazy val server = project
  .settings(name := "example-server")
  .settings(commonSettings)
  .dependsOn(proto)
  .settings(libraryDependencies ++= Seq(
    "dev.tauri" %% "seals-scodec" % sealsVersion,
    "co.fs2" %% "fs2-io" % "2.4.4"
  ))

lazy val example = project.in(file("."))
  .settings(name := "example")
  .settings(commonSettings)
  .aggregate(proto, server)

lazy val commonSettings = Seq[Setting[_]](
  organization := "com.example",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.12.12",
  libraryDependencies ++= Seq(
    "dev.tauri" %% "seals-core" % sealsVersion,
    "org.scalatest" %% "scalatest" % "3.1.4" % Test
  )
)

lazy val sealsVersion = System.getProperty("plugin.version")

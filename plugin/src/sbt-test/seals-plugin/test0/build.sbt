/*
 * Copyright 2016-2017 Daniel Urban and contributors listed in AUTHORS
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

name := "test0"

organization := "com.example"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.11"

crossScalaVersions := Seq(scalaVersion.value, "2.12.3")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)

libraryDependencies += "io.sigs" %% "seals-core" % System.getProperty("plugin.version")

sealsSchemaPackages += "com.example.test0"

mimaPreviousArtifacts := Set(organization.value %% name.value % version.value)

checkExpectedModels := checkExpectedModelsTask.value

lazy val checkExpectedModels = taskKey[Unit]("checkExpectedModels")

lazy val checkExpectedModelsTask = Def.task {
  val log = streams.value.log
  log.info("Reading expected models ...")
  val expected = IO.read(file("expected_models.json")).trim
  log.info("Reading actual models ...")
  val actual = IO.read(sealsSchemaTarget.value / "current.json").trim
  if (expected == actual) {
    log.info("OK, models are the same.")
  } else {
    log.warn("Expected and actual models are different!")
    log.warn("Expected:\n" + expected)
    log.warn("Actual:\n" + actual)
    throw new AssertionError("unexpected model contents")
  }
}

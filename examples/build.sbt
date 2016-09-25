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

lazy val examples = project.in(file("."))
  .settings(name := "seals-examples")
  .settings(exampleSettings)
  .aggregate(invariant, messaging)

lazy val invariant = project
  .settings(name := "seals-examples-invariant")
  .settings(exampleSettings)
  .settings(
    libraryDependencies += "org.spire-math" %% "spire" % "0.12.0"
  )
  .dependsOn(ProjectRef(uri(".."), "core"))
  .dependsOn(ProjectRef(uri(".."), "circe"))

lazy val messaging = project
  .settings(name := "seals-examples-messaging")
  .settings(exampleSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.slf4j" % "slf4j-simple" % "1.7.21"
    )
  )
  .dependsOn(ProjectRef(uri(".."), "core"))
  .dependsOn(ProjectRef(uri(".."), "circe"))

lazy val http4sVersion = "0.14.6"

lazy val exampleSettings = Seq(
  scalaVersion := "2.11.8",
  publishArtifact := false
)

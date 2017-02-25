/*
 * Copyright 2016-2017 Daniel Urban
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

lazy val core = project
  .settings(name := "seals-core")
  .settings(commonSettings)
  .settings(coreSettings)
  .settings(tutSettings)
  .settings(macroSettings)
  .dependsOn(macros)

lazy val macros = project
  .settings(name := "seals-macros")
  .settings(commonSettings)
  .settings(macrosSettings)
  .settings(macroSettings)

lazy val laws = project
  .settings(name := "seals-laws")
  .settings(commonSettings)
  .settings(lawsSettings)
  .dependsOn(core)

lazy val tests = project
  .settings(name := "seals-tests")
  .settings(commonSettings)
  .dependsOn(core, laws % "test->test;compile->compile")

lazy val checker = project
  .settings(name := "seals-checker")
  .settings(commonSettings)
  .settings(checkerSettings)
  .settings(macroSettings)
  .dependsOn(core, circe)

lazy val plugin = project
  .settings(name := "seals-plugin")
  .settings(commonSettings)
  .settings(sbtPluginSettings)
  .enablePlugins(BuildInfoPlugin)

lazy val circe = project
  .settings(name := s"seals-circe")
  .settings(commonSettings)
  .settings(circeSettings)
  .dependsOn(core, laws % "test->test", tests % "test->test")

lazy val scodec = project
  .settings(name := s"seals-scodec")
  .settings(commonSettings)
  .settings(scodecSettings)
  .dependsOn(core, laws % "test->test", tests % "test->test")

lazy val refined = project
  .settings(name := s"seals-refined")
  .settings(commonSettings)
  .settings(refinedSettings)
  .dependsOn(core, laws % "test->test", tests % "test->test")

lazy val seals = project.in(file("."))
  .settings(name := "seals")
  .settings(commonSettings)
  .aggregate(core, macros, laws, tests, checker, circe, scodec, refined) // Note: `plugin` is intentionally missing

lazy val commonSettings = Seq[Setting[_]](
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.1"),
  scalaOrganization := "org.typelevel",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-language:higherKinds,experimental.macros",
    "-Xlint:_",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Xstrict-patmat-analysis",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Ypartial-unification",
    "-Ywarn-unused-import"
  ),
  scalacOptions := scalacOptions.value.flatMap {
    case opt @ "-Xstrict-patmat-analysis" =>
      if (scalaVersion.value.startsWith("2.12")) opt :: Nil
      else Nil
    case opt =>
      opt :: Nil
  },
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused-import" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),

  // We need both of these, due to https://github.com/scalastyle/scalastyle-sbt-plugin/issues/44
  scalastyleConfig in Test := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",
  scalastyleConfig in scalastyle := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",

  libraryDependencies ++= Seq(
    Seq(
      dependencies.cats,
      dependencies.shapeless
    ),
    (dependencies.test ++ dependencies.circe).map(_ % "test-internal")
  ).flatten,
  organization := "io.sigs",
  publishMavenStyle := true,
  publishArtifact := true, // TODO
  mappings in (Compile, packageBin) ++= Seq("LICENSE.txt", "NOTICE.txt") map { f =>
    ((baseDirectory in ThisBuild).value / f) -> f
  },
  TypelevelKeys.githubProject := ("durban", "seals"),
  homepage := Some(url("https://github.com/durban/seals")),
  licenses := Seq("Apache 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
) ++ typelevelDefaultSettings

lazy val coreSettings = Seq[Setting[_]](
  libraryDependencies += dependencies.scodecBits
)

lazy val macrosSettings = Seq[Setting[_]](
  libraryDependencies += dependencies.shapeless % "test-internal"
)

lazy val lawsSettings = Seq[Setting[_]](
  libraryDependencies ++= dependencies.laws
)

lazy val checkerSettings = Seq[Setting[_]](
  libraryDependencies += scalaOrganization.value % "scala-compiler" % scalaVersion.value
)

lazy val macroSettings = Seq[Setting[_]](
  libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)
)

lazy val sbtPluginSettings = scriptedSettings ++ Seq[Setting[_]](
  sbtPlugin := true,
  scalaVersion := "2.10.6",
  crossScalaVersions := Seq(scalaVersion.value),
  scalaOrganization := "org.scala-lang",
  scalacOptions := scalacOptions.value.flatMap {
    case "-Xlint:_" => "-Xlint" :: Nil
    case "-Xstrict-patmat-analysis" => Nil
    case "-Ypartial-unification" => Nil
    case "-Ywarn-unused-import" => Nil
    case opt => opt :: Nil
  },
  addSbtPlugin(dependencies.sbtMima),
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoPackage := "io.sigs.seals.plugin",
  scriptedLaunchOpts ++= Seq("-Dplugin.version=" + version.value, "-Xmx1024M"),
  scriptedBufferLog := false
)

lazy val circeSettings = Seq[Setting[_]](
  libraryDependencies ++= dependencies.circe
)

lazy val scodecSettings = Seq[Setting[_]](
  libraryDependencies ++= dependencies.scodec
)

lazy val refinedSettings = Seq[Setting[_]](
  libraryDependencies ++= Seq(
    dependencies.refined,
    scalaOrganization.value % "scala-compiler" % scalaVersion.value % "test-internal"
  )
)

lazy val dependencies = new {

  val catsVersion = "0.9.0"
  val circeVersion = "0.7.0"

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.2"
  val cats = "org.typelevel" %% "cats-core" % catsVersion

  val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  val scodecBits = "org.scodec" %% "scodec-bits" % "1.1.4"
  val scodecCats = "org.scodec" %% "scodec-cats" % "0.3.0"
  val scodec = Seq(
    scodecBits,
    "org.scodec" %% "scodec-core" % "1.10.3",
    "org.scodec" %% "scodec-stream" % "1.0.1",
    scodecCats
  )

  val refined = "eu.timepit" %% "refined" % "0.7.0"

  val laws = Seq(
    scodecCats,
    "org.typelevel" %% "cats-laws" % catsVersion,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.4"
  )

  val test = Seq(
    "org.scalatest" %% "scalatest" % "3.0.1"
  )

  val sbtMima = "com.typesafe" % "sbt-mima-plugin" % "0.1.13"
}

addCommandAlias("testAll", ";test;examples/test")
addCommandAlias("scalastyleAll", ";scalastyle;test:scalastyle;examples/scalastyle;examples/test:scalastyle")
addCommandAlias("tutAll", "core/tut")
addCommandAlias("doAll", ";testAll;scalastyleAll;tutAll;publishLocal")

addCommandAlias("validate", ";clean;++ 2.11.8;doAll;++ 2.12.1;doAll;++ 2.10.6;plugin/scripted;reload")


//////////////////////
///    Examples    ///
//////////////////////

lazy val examples = project.in(file("examples"))
  .settings(name := "seals-examples")
  .settings(exampleSettings)
  .aggregate(exInvariant, exMessaging, exStreaming, exLib)

lazy val exInvariant = project.in(file("examples/invariant"))
  .settings(name := "seals-examples-invariant")
  .settings(exampleSettings)
  .settings(libraryDependencies += exampleDependencies.spire)
  .dependsOn(core, circe)

lazy val exMessaging = project.in(file("examples/messaging"))
  .settings(name := "seals-examples-messaging")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.http4s)
  .dependsOn(core, circe)

lazy val exStreaming = project.in(file("examples/streaming"))
  .settings(name := "seals-examples-streaming")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.fs2)
  .dependsOn(core, scodec)

lazy val exLib = project.in(file("examples/lib"))
  .settings(name := "seals-examples-lib")
  .settings(exampleSettings)
  .aggregate(exLibProto, exLibServer, exLibClient)

lazy val exLibProto = project.in(file("examples/lib/proto"))
  .settings(name := "seals-examples-lib-proto")
  .settings(exampleSettings)
  .settings(macroSettings)
  .dependsOn(core)

lazy val exLibServer = project.in(file("examples/lib/server"))
  .settings(name := "seals-examples-lib-server")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.fs2)
  .dependsOn(core, scodec, exLibProto)

lazy val exLibClient = project.in(file("examples/lib/client"))
  .settings(name := "seals-examples-lib-client")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.akka)
  .settings(exampleDependencies.streamz)
  .dependsOn(core, scodec, exLibProto, exLibServer % "test->compile")

lazy val exampleSettings = Seq(
  scalaVersion := "2.11.8",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-Xlint:_",
    "-Xfuture",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Ywarn-unused-import"
  ),
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused-import" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  libraryDependencies ++= dependencies.test.map(_ % "test-internal"),

  // We need all three, due to https://github.com/scalastyle/scalastyle-sbt-plugin/issues/44
  scalastyleConfig in Compile := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",
  scalastyleConfig in Test := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",
  scalastyleConfig in scalastyle := (baseDirectory in ThisBuild).value / "scalastyle-test-config.xml",

  publishArtifact := false
)

lazy val exampleDependencies = new {

  val http4sVersion = "0.15.4a"

  val http4s = Seq(
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "org.slf4j" % "slf4j-simple" % "1.7.22"
  )

  val spire = "org.spire-math" %% "spire" % "0.13.0"

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % "0.9.3",
    "co.fs2" %% "fs2-io" % "0.9.3"
  )

  val akka = Seq(
    "com.typesafe.akka" %% "akka-stream" % "2.4.17",
    "org.scodec" %% "scodec-akka" % "0.3.0"
  )

  val streamz = Seq[Setting[_]](
    resolvers += "krasserm" at "http://dl.bintray.com/krasserm/maven",
    libraryDependencies += "com.github.krasserm" %% "streamz-converter" % "0.7"
  )
}

/*
 * Copyright 2016-2020 Daniel Urban and contributors listed in AUTHORS
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

lazy val core = project
  .settings(name := "seals-core")
  .settings(commonSettings)
  .settings(coreSettings)
  .settings(macroSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(macros)

lazy val doc = project
  .settings(name := "seals-doc")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(scalacOptions -= "-Xfatal-warnings")
  .settings(scalacOptions -= "-Werror")
  .enablePlugins(MdocPlugin)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core)

lazy val macros = project
  .settings(name := "seals-macros")
  .settings(commonSettings)
  .settings(macrosSettings)
  .settings(macroSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514

lazy val laws = project
  .settings(name := "seals-laws")
  .settings(commonSettings)
  .settings(lawsSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core)

lazy val tests = project
  .settings(name := "seals-tests")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, laws % "test->test;compile->compile")

lazy val checker = project
  .settings(name := "seals-checker")
  .settings(commonSettings)
  .settings(checkerSettings)
  .settings(macroSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, circe)

lazy val plugin = project
  .settings(name := "seals-plugin")
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(ScriptedPlugin)
  .settings(commonSettings)
  .settings(pluginSettings)
  .settings(scriptedDependencies := {
    scriptedDependencies.value
    // workaround for https://github.com/sbt/sbt/issues/3248
    (checker / publishLocal).value
    (core / publishLocal).value
    (circe / publishLocal).value
    (macros / publishLocal).value
    (scodec / publishLocal).value // due to the scripted test "seals-plugin / example"
  })

lazy val circe = project
  .settings(name := s"seals-circe")
  .settings(commonSettings)
  .settings(circeSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, laws % "test->test", tests % "test->test")

lazy val scodec = project
  .settings(name := s"seals-scodec")
  .settings(commonSettings)
  .settings(scodecSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, laws % "test->test", tests % "test->test")

lazy val refined = project
  .settings(name := s"seals-refined")
  .settings(commonSettings)
  .settings(refinedSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, laws % "test->test", tests % "test->test")

lazy val seals = project.in(file("."))
  .settings(name := "seals")
  .settings(commonSettings)
  .settings(noPublishSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .aggregate(core, macros, laws, tests, checker, circe, scodec, refined) // Note: `plugin` is intentionally missing

lazy val consts = new {
  val githubOrg = "durban"
  val githubProject = "seals"
  val additionalFiles = Seq("LICENSE.txt", "NOTICE.txt", "AUTHORS")
}

lazy val commonSettings: Seq[Setting[_]] = Seq[Setting[_]](
  scalaVersion := "2.13.3",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.12"),
  scalaOrganization := "org.scala-lang",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-language:higherKinds,experimental.macros",
    "-Xmigration:2.13.3",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    // TODO: set -sourcepath and -doc-source-url
    // TODO: probably also set autoAPIMappings and apiURL
  ),
  scalacOptions ++= {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, 12)) =>
        Seq(
          "-Xfatal-warnings",
          "-Xlint:-unused,_",
          "-Ypartial-unification",
          "-Yno-adapted-args",
        )
      case _ =>
        Seq(
          "-Werror",
          "-Wunused:_",
          "-Wmacros:after",
          "-Wvalue-discard",
          "-Xlint:-unused,-byname-implicit,_",
        )
    }
  },
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused-import" == _).filterNot("-Ywarn-unused:imports" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.2" cross CrossVersion.full),

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
  organization := "dev.tauri",
  organizationHomepage := Some(url("https://tauri.dev")),
  homepage := Some(url(s"https://github.com/${consts.githubOrg}/${consts.githubProject}")),
  scmInfo := Some(ScmInfo(
    url(s"https://github.com/${consts.githubOrg}/${consts.githubProject}"),
    s"scm:git@github.com:${consts.githubOrg}/${consts.githubProject}.git"
  )),
  developers += Developer(
    id = "durban",
    name = "Daniel Urban",
    email = "urban.dani@gmail.com",
    url = url("https://github.com/durban")
  ),
  licenses := Seq("Apache 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
) ++ inConfig(Compile)(
  inTask(packageBin)(extraPackagingSettings) ++
  inTask(packageSrc)(extraPackagingSettings) ++
  inTask(packageDoc)(extraPackagingSettings)
) ++ publishSettings

lazy val extraPackagingSettings = Seq[Setting[_]](
  mappings ++= consts.additionalFiles map { f =>
    ((baseDirectory in ThisBuild).value / f) -> f
  },
  packageOptions += Package.ManifestAttributes(java.util.jar.Attributes.Name.SEALED -> "true")
)

lazy val publishSettings = Seq[Setting[_]](
  publishMavenStyle := true,
  publishArtifact in Compile := true,
  publishArtifact in Test := false,
  pomIncludeRepository := { _ => false },
  publishTo := {
    if (isSnapshot.value) Some("snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")
    else Some("staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
  },
  releaseCrossBuild := true,
  releaseVcsSign := true,
  releasePublishArtifactsAction := com.typesafe.sbt.pgp.PgpKeys.publishSigned.value,
  releaseProcess := {
    import ReleaseTransformations._
    Seq[ReleaseStep](
      releaseStepCommandAndRemaining("clean"),
      checkSnapshotDependencies,
      inquireVersions,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepCommandAndRemaining("plugin/clean"), // workaround for https://github.com/sbt/sbt-buildinfo/issues/108
      releaseStepCommandAndRemaining("plugin/publishSigned"),
      publishArtifacts,
      setNextVersion,
      commitNextVersion
    )
  },
  useGpg := true,
  useGpgAgent := true,
  com.typesafe.sbt.pgp.PgpKeys.gpgCommand in Global := "gpg2"
)

lazy val noPublishSettings = Seq[Setting[_]](
  skip in publish := true,
  publishArtifact in Compile := false,
  publishMavenStyle := false
)

lazy val coreSettings = Seq[Setting[_]](
  libraryDependencies += dependencies.scodecBits,
  libraryDependencies ++= {
    if (scalaVersion.value.startsWith("2.12.")) {
      List(dependencies.collectionCompat)
    } else {
      List()
    }
  }
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
  libraryDependencies ++= {
    if (scalaVersion.value.startsWith("2.12.")) {
      List(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
    } else {
      List()
    }
  },
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.12.")) {
      List()
    } else {
      List("-Ymacro-annotations")
    }
  }
)

lazy val pluginSettings = Seq[Setting[_]](
  sbtPlugin := true,
  scalaVersion := "2.12.12",
  crossScalaVersions := Seq(scalaVersion.value),
  scalaOrganization := "org.scala-lang",
  libraryDependencies += Defaults.sbtPluginExtra(
    dependencies.sbtMima,
    (sbtBinaryVersion in pluginCrossBuild).value,
    (scalaBinaryVersion in update).value
  ),
  buildInfoKeys := Seq[BuildInfoKey](version),
  buildInfoPackage := "dev.tauri.seals.plugin",
  releaseCrossBuild := false,
  scriptedLaunchOpts ++= Seq("-Dplugin.version=" + version.value, "-Xmx1024M"),
  scriptedBufferLog := false
)

lazy val circeSettings = Seq[Setting[_]](
  libraryDependencies ++= dependencies.circe,
  libraryDependencies += dependencies.circeTesting % "test-internal"
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

  val catsVersion = "2.3.0"
  val circeVersion = "0.14.0-M1"

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.3"
  val cats = "org.typelevel" %% "cats-core" % catsVersion
  val collectionCompat = "org.scala-lang.modules" %% "scala-collection-compat" % "2.3.1"

  val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  val circeTesting = "io.circe" %% "circe-testing" % circeVersion

  val scodecBits = "org.scodec" %% "scodec-bits" % "1.1.22"
  val scodecCats = "org.scodec" %% "scodec-cats" % "1.1.0-M3"
  val scodec = Seq(
    scodecBits,
    "org.scodec" %% "scodec-core" % "1.11.7",
    "org.scodec" %% "scodec-stream" % "2.0.0",
    scodecCats
  )

  val refined = "eu.timepit" %% "refined" % "0.9.19"

  val laws = Seq(
    scodecCats,
    "org.typelevel" %% "cats-laws" % catsVersion,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"
  )

  val test = Seq(
    "org.scalatest" %% "scalatest" % "3.1.4",
    "org.typelevel" %% "discipline-scalatest" % "1.0.0-RC4"
  )

  val sbtMima = "com.typesafe" % "sbt-mima-plugin" % "0.8.1"
}

addCommandAlias("testAll", ";test;examples/test")
addCommandAlias("scalastyleAll", ";scalastyle;test:scalastyle;examples/scalastyle;examples/test:scalastyle")
addCommandAlias("mdocAll", "doc/mdoc")
addCommandAlias("doAll", ";testAll;scalastyleAll;mdocAll;publishLocal")
addCommandAlias("doPlugin", ";plugin/clean;plugin/test;plugin/scalastyle;plugin/test:scalastyle;plugin/scripted")

addCommandAlias("validate", ";clean;+ doAll;doPlugin;reload")


//////////////////////
///    Examples    ///
//////////////////////

lazy val examples = project.in(file("examples"))
  .settings(name := "seals-examples")
  .settings(exampleSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .aggregate(exInvariant, exMessaging, exStreaming, exLib)

lazy val exInvariant = project.in(file("examples/invariant"))
  .settings(name := "seals-examples-invariant")
  .settings(exampleSettings)
  .settings(libraryDependencies += exampleDependencies.spire)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, circe)

lazy val exMessaging = project.in(file("examples/messaging"))
  .settings(name := "seals-examples-messaging")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= (exampleDependencies.http4s :+ exampleDependencies.catsEffect))
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, circe)

lazy val exStreaming = project.in(file("examples/streaming"))
  .settings(name := "seals-examples-streaming")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.fs2)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, scodec)

lazy val exLib = project.in(file("examples/lib"))
  .settings(name := "seals-examples-lib")
  .settings(exampleSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .aggregate(exLibProto, exLibServer, exLibClient)

lazy val exLibProto = project.in(file("examples/lib/proto"))
  .settings(name := "seals-examples-lib-proto")
  .settings(exampleSettings)
  .settings(macroSettings)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core)

lazy val exLibServer = project.in(file("examples/lib/server"))
  .settings(name := "seals-examples-lib-server")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.fs2)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, scodec, exLibProto)

lazy val exLibClient = project.in(file("examples/lib/client"))
  .settings(name := "seals-examples-lib-client")
  .settings(exampleSettings)
  .settings(libraryDependencies ++= exampleDependencies.akka)
  .disablePlugins(ScriptedPlugin) // workaround for https://github.com/sbt/sbt/issues/3514
  .dependsOn(core, scodec, exLibProto, exLibServer % "test->compile;test->test")

lazy val exampleSettings = Seq(
  scalaVersion := "2.13.4",
  crossScalaVersions := Seq(scalaVersion.value, "2.12.12"),
  scalaOrganization := "org.scala-lang",
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Ywarn-unused:imports",
    "-Xmigration:2.13.13"
  ),
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.12.")) List(
      "-Yno-adapted-args",
      "-Xlint:_"
     ) else List(
      "-Xlint:-byname-implicit,_"
     )
  },
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

  val http4sVersion = "0.21.14"
  val fs2Version = "2.4.6"

  val http4s = Seq(
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "org.slf4j" % "slf4j-simple" % "1.7.25"
  )

  val spire = "org.typelevel" %% "spire" % "0.17.0"

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version
  )

  val catsEffect = "org.typelevel" %% "cats-effect" % "2.3.0"

  val akka = Seq(
    "com.typesafe.akka" %% "akka-stream" % "2.6.10",
    "co.fs2" %% "fs2-reactive-streams" % fs2Version
  )
}

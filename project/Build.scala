import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.0.0"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "ch.epfl.lamp",
    version := "0.1-SNAPSHOT",
    scalacOptions ++= Seq("-Xlog-implicits", "-deprecation"),
    autoAPIMappings := true,
    scalaVersion := "2.11.0",
    crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.1.5" % "test",
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root = Project(
    "root",
    file("."),
    settings = buildSettings ++ Seq(
      name := "dep",
      run <<= run in Compile in core,
      console <<= console in Compile in core
    )
  ) aggregate(macros, core, common)

  lazy val common = Project(
    "common",
    file("common"),
    settings = buildSettings
  )

  lazy val macros: Project = Project(
    "macros",
    file("macros"),
    settings = buildSettings ++ Seq(
      scalacOptions += "-language:experimental.macros",
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= (
        if (scalaVersion.value.startsWith("2.10")) List("org.scalamacros" %% "quasiquotes" % paradiseVersion)
        else Nil
      )
    )
  ) dependsOn(common)

  lazy val core = Project(
    "core",
    file("core"),
    settings = buildSettings
  ) dependsOn(macros, common)
}

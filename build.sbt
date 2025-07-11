val scala3Version = "3.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sc_poker",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",

      scalacOptions += "-explain"
  )

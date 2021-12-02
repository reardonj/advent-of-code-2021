val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    fork := true,
    run / connectInput := true,
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.2.0",
    libraryDependencies += "co.fs2" %% "fs2-io" % "3.2.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )

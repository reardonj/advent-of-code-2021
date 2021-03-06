val scala3Version = "3.1.0"
run / javaOptions += "-Xmx8G"
lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-code-2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    fork := true,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"
  )

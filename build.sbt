val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "functional-programming-in-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    scalacOptions ++= List(
      "-feature",
      "-deprecation",
      "-Ykind-projector:underscores",
      "-source:future"
    ),

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
val scala3Version = "3.8.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "algos-ds-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.3.0" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.3.0" % Test
    )
  )

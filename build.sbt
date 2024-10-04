val scalaVer = "3.5.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graphics-scala",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scalaVer,
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )

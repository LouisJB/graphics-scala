val scalaVer = "3.7.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "graphics-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scalaVer,
    scalacOptions ++= Seq(
      "-unchecked",
      "-deprecation",
      "-feature",
      "-language:implicitConversions",
      "-language:existentials",
      "-Werror",
    ),
    libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
  )

val junit = "junit" % "junit" % "4.12" % "test"
val scalatest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"

lazy val commonSettings = Seq(
  organization := "org.monkeynuthead",
  version := "0.1.0",
  scalaVersion := "2.11.6",
  sbtVersion := "0.13.8",
  scalacOptions ++= Seq("-feature","-unchecked","-deprecation")
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "scala-playground",
    libraryDependencies ++= Seq(
      junit, scalatest
    )
  )

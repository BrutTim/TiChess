ThisBuild / scalaVersion := "3.3.3"
ThisBuild / organization := "ch.tichess"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "TiChess",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ),
    Test / fork := true
  )

ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "ch.tichess"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / coverageExcludedFiles := ".*GuiMain|.*FastParseParsers.*|.*GuiViewAdapter.*|.*RestServer.*|.*services/.*|.*MongoGameDao.*"

lazy val javaFxVersion = "21.0.2"
lazy val osName = sys.props("os.name").toLowerCase
lazy val osArch = sys.props("os.arch").toLowerCase
lazy val isArm64 = osArch.contains("aarch64") || osArch.contains("arm64")
lazy val javaFxPlatform =
  sys.env.get("JAVAFX_PLATFORM").getOrElse {
    if (osName.contains("mac")) {
      if (isArm64) "mac-aarch64" else "mac"
    } else if (osName.contains("win")) {
      "win"
    } else if (osName.contains("linux")) {
      if (isArm64) "linux-aarch64" else "linux"
    } else {
      "linux"
    }
  }

lazy val javaFxModules = Seq("base", "graphics", "controls", "fxml")
lazy val includeJavaFx = sys.env.get("INCLUDE_JAVAFX").forall(_.toLowerCase != "false")
lazy val javaFxDependencies =
  if (includeJavaFx)
    javaFxModules.map { m =>
      ("org.openjfx" % s"javafx-$m" % javaFxVersion).classifier(javaFxPlatform)
    }
  else Seq.empty

lazy val root = (project in file("."))
  .settings(
    name := "TiChess",
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "21.0.0-R32",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.5",
      "com.typesafe.akka" %% "akka-stream" % "2.8.5",
      "com.typesafe.akka" %% "akka-http" % "10.5.3",
      "com.typesafe.akka" %% "akka-http-spray-json" % "10.5.3",
      "com.typesafe.akka" %% "akka-stream-testkit" % "2.8.5" % Test,
      "com.typesafe.akka" %% "akka-http-testkit" % "10.5.3" % Test,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.8.5" % Test,
      "com.typesafe.slick" %% "slick" % "3.5.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1",
      "org.postgresql" % "postgresql" % "42.7.4",
      "com.h2database" % "h2" % "2.3.232" % Test,
      "org.slf4j" % "slf4j-nop" % "2.0.16",
      ("org.mongodb.scala" %% "mongo-scala-driver" % "5.2.0").cross(CrossVersion.for3Use2_13),
      "org.scalatestplus" %% "mockito-4-11" % "3.2.18.0" % Test
    ) ++ javaFxDependencies,
    Test / fork := true
  )

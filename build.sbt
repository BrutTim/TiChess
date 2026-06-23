ThisBuild / scalaVersion := "3.3.4"
ThisBuild / organization := "ch.tichess"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / coverageExcludedFiles := ".*Main.*|.*GuiMain|.*FastParseParsers.*|.*GuiViewAdapter.*|.*RestServer.*|.*ModelServer.*|.*ControllerServer.*|.*HttpModelService.*|.*ControllerHttpClient.*|.*ServiceConfig.*|.*bot/AlphaBetaBot.*|.*bot/lichess/.*|.*streaming/Kafka.*|.*streaming/StreamServer.*|.*MongoGameDao.*|.*MongoChallengeDao.*"
ThisBuild / assemblyRepeatableBuild := true

lazy val javaFxVersion = "21.0.2"
lazy val fatJar = sbtassembly.AssemblyPlugin.autoImport.assembly
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
lazy val sparkJavaOptions = Seq("--add-exports=java.base/sun.nio.ch=ALL-UNNAMED")
lazy val javaFxDependencies =
  if (includeJavaFx)
    javaFxModules.map { m =>
      ("org.openjfx" % s"javafx-$m" % javaFxVersion).classifier(javaFxPlatform)
    }
  else Seq.empty

lazy val root = (project in file("."))
  .enablePlugins(JmhPlugin, GatlingPlugin)
  .settings(
    name := "TiChess",
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "21.0.0-R32",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
      "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "com.typesafe.akka" %% "akka-actor-typed" % "2.8.5",
      "com.typesafe.akka" %% "akka-stream" % "2.8.5",
      "com.typesafe.akka" %% "akka-http" % "10.5.3",
      "com.typesafe.akka" %% "akka-http-spray-json" % "10.5.3",
      "com.typesafe.akka" %% "akka-stream-kafka" % "4.0.2",
      ("org.apache.spark" %% "spark-sql" % "3.5.5")
        .cross(CrossVersion.for3Use2_13)
        .excludeAll(
          ExclusionRule(organization = "org.scala-lang.modules", name = "scala-parser-combinators_2.13"),
          ExclusionRule(organization = "org.scala-lang.modules", name = "scala-xml_2.13")
        ),
      ("org.apache.spark" %% "spark-sql-kafka-0-10" % "3.5.5")
        .cross(CrossVersion.for3Use2_13)
        .excludeAll(
          ExclusionRule(organization = "org.scala-lang.modules", name = "scala-parser-combinators_2.13"),
          ExclusionRule(organization = "org.scala-lang.modules", name = "scala-xml_2.13")
        ),
      "com.typesafe.akka" %% "akka-stream-testkit" % "2.8.5" % Test,
      "com.typesafe.akka" %% "akka-http-testkit" % "10.5.3" % Test,
      "com.typesafe.akka" %% "akka-actor-testkit-typed" % "2.8.5" % Test,
      "com.typesafe.slick" %% "slick" % "3.5.1",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.5.1",
      "org.postgresql" % "postgresql" % "42.7.4",
      "com.h2database" % "h2" % "2.3.232" % Test,
      "org.slf4j" % "slf4j-nop" % "2.0.16",
      ("org.mongodb.scala" %% "mongo-scala-driver" % "5.2.0").cross(CrossVersion.for3Use2_13),
      "org.scalatestplus" %% "mockito-4-11" % "3.2.18.0" % Test,
      "io.gatling.highcharts" % "gatling-charts-highcharts" % "3.11.5" % Test excludeAll(ExclusionRule(organization = "com.typesafe.akka"), ExclusionRule(organization = "org.scala-lang.modules")),
      "io.gatling" % "gatling-test-framework" % "3.11.5" % Test excludeAll(ExclusionRule(organization = "com.typesafe.akka"), ExclusionRule(organization = "org.scala-lang.modules"))
    ) ++ javaFxDependencies,
    dependencyOverrides ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-annotations" % "2.15.2",
      "com.fasterxml.jackson.core" % "jackson-core" % "2.15.2",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.15.2",
      ("com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.15.2")
        .cross(CrossVersion.for3Use2_13)
    ),
    Compile / unmanagedResources / excludeFilter :=
      GlobFilter("*.rtbw") || GlobFilter("*.rtbz") || HiddenFileFilter,
    fatJar / assemblyJarName := "tichess.jar",
    fatJar / test := {},
    fatJar / assemblyMergeStrategy := {
      case "module-info.class"                    => MergeStrategy.discard
      case "version.conf"                         => MergeStrategy.first
      case "application.conf" | "reference.conf" => MergeStrategy.concat
      case PathList("META-INF", "services", _*)   => MergeStrategy.concat
      case PathList("META-INF", _*)               => MergeStrategy.discard
      case path                                   => (ThisBuild / assemblyMergeStrategy).value(path)
    },
    Test / fork := true,
    Test / javaOptions ++= sparkJavaOptions,
    Compile / run / fork := true,
    Compile / run / javaOptions ++= sparkJavaOptions,
    Test / parallelExecution := false,
    Test / test := (Test / test).dependsOn(Compile / copyResources).value
  )

ThisBuild / scalaVersion := "3.3.3"
ThisBuild / organization := "ch.tichess"
ThisBuild / version := "0.1.0-SNAPSHOT"

lazy val javaFxVersion = "21.0.2"
lazy val osName = sys.props("os.name").toLowerCase
lazy val osArch = sys.props("os.arch").toLowerCase
lazy val isArm64 = osArch.contains("aarch64") || osArch.contains("arm64")
lazy val javaFxPlatform =
  if (osName.contains("mac")) {
    if (isArm64) "mac-aarch64" else "mac"
  } else if (osName.contains("win")) {
    "win"
  } else if (osName.contains("linux")) {
    if (isArm64) "linux-aarch64" else "linux"
  } else {
    "linux"
  }

lazy val javaFxModules = Seq("base", "graphics", "controls", "fxml")

lazy val root = (project in file("."))
  .settings(
    name := "TiChess",
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "21.0.0-R32",
      "org.scalatest" %% "scalatest" % "3.2.19" % Test
    ) ++ javaFxModules.map(m => "org.openjfx" % s"javafx-$m" % javaFxVersion classifier javaFxPlatform),
    Test / fork := true
  )

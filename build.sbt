name := "vapors-root"
ThisBuild / organization := "com.rallyhealth"

ThisBuild / scalaVersion := Dependencies.Scala_2_13

ThisBuild / scalacOptions ++= Seq(
  "-deprecation:false",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-Xfatal-warnings",
  "-Ymacro-annotations",
)

def commonProject(dir: String): Project = {
  Project(dir, file(dir)).settings(
    name := s"vapors-$dir",
  )
}

lazy val core = commonProject("core")
  .settings(
    libraryDependencies ++= Dependencies.CoreProject.all(scalaVersion.value),
  )

lazy val circe = commonProject("circe")
  .dependsOn(core)
  .settings(
    libraryDependencies ++= Dependencies.CirceProject.all,
  )

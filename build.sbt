import Dependencies._

name := "vapors-root"
ThisBuild / organization := "com.rallyhealth"
ThisBuild / organizationName := "Rally Health"

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := Dependencies.Scala_3
ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

ThisBuild / scalacOptions ++= Seq(
  "-deprecation:false",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-Xfatal-warnings",
)

// reload sbt when the build files change
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / homepage := Some(url("https://github.com/jeffmay/vapors"))
ThisBuild / developers := List(
  Developer(id = "jeffmay", name = "Jeff May", email = "jeff.n.may@gmail.com", url = url("https://github.com/jeffmay")),
)

// ScalaDoc generation is generally broken. It's really mostly useful from within the IDE anyway
// so just disable generation to allow publishing without ScalaDoc errors.
ThisBuild / packageDoc / publishArtifact := false

// Disable publishing of the root project
publish / skip := true

def commonProject(
  dir: String,
  projectPrefix: String = "",
): Project = {
  val packagePrefix = s"com.rallyhealth${if (projectPrefix.isEmpty) "" else s".$projectPrefix"}"
  Project(dir, file(dir))
    .settings(
      name := s"vapors-$dir",
      idePackagePrefix.withRank(KeyRanks.Invisible) := Some(packagePrefix),
    )
}

val deps = new {
  val zio = "dev.zio" %% "zio" % "2.0.2"
  val zioPrelude = "dev.zio" %% "zio-prelude" % "1.0.0-RC16"
  val zioTest = "dev.zio" %% "zio-test" % zio.revision
}

lazy val `core-v2` = commonProject("core-v2", "vapors.v2")
  .settings(
    libraryDependencies ++= Seq(
      deps.zio,
      deps.zioPrelude,
    ) ++ Seq(
      // Test-only dependencies
      deps.zioTest,
    ).map(_ % Test)
  )

//lazy val `core-v1` = commonProject("core-v1", "vapors.v1")
//  .settings(
//    libraryDependencies ++= CoreV1Project.all,
//  )

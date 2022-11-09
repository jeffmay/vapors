import Dependencies._

name := "vapors-root"
ThisBuild / organization := "com.rallyhealth"
ThisBuild / organizationName := "Rally Health"
ThisBuild / homepage := Some(url("https://github.com/jeffmay/vapors"))
ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/jeffmay/vapors"), "scm:git:git@github.com:jeffmay/vapors.git"),
)
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := Dependencies.Scala_2_13
ThisBuild / licenses := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))

ThisBuild / scalacOptions ++= Seq(
  "-deprecation:false",
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-opt-warnings:at-inline-failed-summary",
  "-unchecked",
  "-Xlint:doc-detached",
  "-Xlint:eta-sam",
  "-Xlint:eta-zero",
  "-Xlint:deprecation",
  "-Xlint:nonlocal-return",
  "-Xlint:nullary-unit",
  "-Xlint:package-object-classes",
  "-Xlint:private-shadow",
  "-Xlint:implicit-not-found",
  "-Xlint:type-parameter-shadow",
  "-Xlint:valpattern",
  "-Xlog-implicits",
  "-Werror",
  "-Wself-implicit",
  "-Wunused:privates",
  "-Wunused:locals",
  "-Wvalue-discard",
  "-Ymacro-annotations",
)
ThisBuild / Compile / scalacOptions --= Seq(
  "-Wunused:privates",
  "-Wunused:locals",
  "-Wvalue-discard",
)

// reload sbt when the build files change
Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / homepage := Some(url("https://github.com/jeffmay/vapors"))
ThisBuild / developers := List(
  Developer(id = "jeffmay", name = "Jeff May", email = "jeff.n.may@gmail.com", url = url("https://github.com/jeffmay")),
)

// Disable publishing of the root project
publish / skip := true

// SBT CI Release Plugin
ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.StartsWith(Ref.Tag("v")))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env = Map(
      "PGP_PASSPHRASE" -> "${{ secrets.PGP_PASSPHRASE }}",
      "PGP_SECRET" -> "${{ secrets.PGP_SECRET }}",
      "SONATYPE_PASSWORD" -> "${{ secrets.SONATYPE_PASSWORD }}",
      "SONATYPE_USERNAME" -> "${{ secrets.SONATYPE_USERNAME }}",
    ),
  ),
)

def commonProject(
  dir: String,
  projectPrefix: String = "",
): Project = {
  val packagePrefix = s"com.rallyhealth${if (projectPrefix.isEmpty) "" else s".$projectPrefix"}"
  Project(dir, file(dir))
    .settings(
      name := s"vapors-$dir",
      idePackagePrefix.withRank(KeyRanks.Invisible) := Some(packagePrefix),
      Compile / doc / scalacOptions += "--no-link-warnings",
      Test / doc / scalacOptions += "--no-link-warnings",
      addCompilerPlugin(("org.typelevel" % "kind-projector" % "0.13.2").cross(CrossVersion.full)),
    )
}

lazy val bench = commonProject("bench", "vapors")
  .dependsOn(`core-v1` % "test->test") // TODO: Include other projects for comparison
  .settings(
    publish / skip := true,
    libraryDependencies ++= BenchProject.all,
    Test / parallelExecution := false,
  )

lazy val `core-v1` = commonProject("core-v1", "vapors.v1")
  .settings(
    libraryDependencies ++= CoreV1Project.all,
  )

lazy val `circe-v1` = commonProject("circe-v1", "vapors.v1")
  .dependsOn(`core-v1` % "compile;test->test")
  .settings(
    libraryDependencies ++= CirceV1Project.all,
  )

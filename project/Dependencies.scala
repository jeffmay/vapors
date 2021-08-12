import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.6"

  private val alleyCatsCore = "org.typelevel" %% "alleycats-core" % "2.6.1"
  private val catsCore = "org.typelevel" %% "cats-core" % "2.6.1"
  private val catsFree = "org.typelevel" %% "cats-free" % "2.6.1"
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.15.4"
  private val scalaCheckOps = "com.rallyhealth" %% "scalacheck-ops_1-15" % "2.6.0"
  private val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"
  private val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0"
  private def scalaReflect(scalacVersion: String): ModuleID = "org.scala-lang" % "scala-reflect" % scalacVersion
  private val shapeless = "com.chuusai" %% "shapeless" % "2.3.7"

  final object Plugins {

    val kindProjector = {
      compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0").cross(CrossVersion.full)
    }
  }

  final object CoreProject {

    def all(scalaVersion: String): Seq[ModuleID] =
      Seq(
        Plugins.kindProjector,
        alleyCatsCore,
        catsCore,
        catsFree,
        scalaReflect(scalaVersion),
        shapeless,
      ) ++ Seq(
        // Test-only dependencies
        scalaCheck,
        scalaCheckOps,
        scalaTest,
        scalaTestPlusScalaCheck,
      ).map(_ % Test)
  }

}

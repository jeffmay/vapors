import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.0"

  private final val catsVersion = "2.1.1"
  private final val circeVersion = "0.13.0"
  private final val kindProjectorVersion = "0.11.0"
  private final val scalaCheckVersion = "1.14.3"
  private final val scalaTestVersion = "3.2.2"
  private final val scalaTestPlusScalaCheckVersion = "3.2.2.0"

  private val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  private val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  private val circeCore = "io.circe" %% "circe-core" % circeVersion
  private val circeParser = "io.circe" %% "circe-parser" % circeVersion
  private val circeLiteral = "io.circe" %% "circe-literal" % circeVersion
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  private val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  private def scalaReflect(scalacVersion: String): ModuleID = "org.scala-lang" % "scala-reflect" % scalacVersion

  final object Plugins {

    val kindProjector = {
      compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    }
  }

  final object Resolvers {

    val danslapman: Resolver = Resolver.bintrayRepo("danslapman", "maven")
  }

  final object CoreProject {

    def all(scalaVersion: String): Seq[ModuleID] =
      Seq(
        Plugins.kindProjector,
        catsCore,
        catsFree,
        scalaReflect(scalaVersion),
      ) ++ Seq(
        // Test-only dependencies
        scalaCheck,
        scalaTest,
      ).map(_ % Test)
  }

  final object CirceProject {

    val all: Seq[ModuleID] = Seq(
      Plugins.kindProjector,
      circeCore,
    ) ++ Seq(
      // Test-only dependencies
      circeLiteral,
      circeParser,
      scalaCheck,
      scalaTest,
    ).map(_ % Test)
  }

}

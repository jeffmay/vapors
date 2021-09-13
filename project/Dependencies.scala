import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.6"

  private final val catsVersion = "2.6.1"
  private final val catsEffectVersion = "3.2.7"
  private final val circeVersion = "0.14.1"
  private final val izumiReflectVersion = "1.1.3"
  private final val kindProjectorVersion = "0.13.0"
  private final val munitVersion = "0.7.28"
  private final val munitCatsEffectVersion = "1.0.5"
  private final val scalacticVersion = "3.2.9"
  private final val scalaCheckVersion = "1.15.4"
  private final val scalaCheckOpsVersion = "2.7.1"
  private final val scalaTestVersion = "3.2.9"
  private final val scalaTestPlusScalaCheckVersion = "3.2.9.0"
  private final val shapelessVersion = "2.3.7"
  private final val sourcecodeVersion = "0.2.7"
  private final val zioVersion = "1.0.11"

  private val alleyCatsCore = "org.typelevel" %% "alleycats-core" % catsVersion
  private val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  private val catsEffect = "org.typelevel" %% "cats-effect" % catsEffectVersion
  private val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  private val circeCore = "io.circe" %% "circe-core" % circeVersion
  private val circeLiteral = "io.circe" %% "circe-literal" % circeVersion
  private val circeGeneric = "io.circe" %% "circe-generic" % circeVersion
  private val circeParser = "io.circe" %% "circe-parser" % circeVersion
  private val izumiReflect = "dev.zio" %% "izumi-reflect" % izumiReflectVersion
  private val munit = "org.scalameta" %% "munit" % munitVersion
  private val munitCatsEffect = "org.typelevel" %% "munit-cats-effect-3" % munitCatsEffectVersion
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  private val scalaCheckOps = "com.rallyhealth" %% "scalacheck-ops_1-15" % scalaCheckOpsVersion
  private val scalactic = "org.scalactic" %% "scalactic" % scalacticVersion
  private val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  private val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalaTestPlusScalaCheckVersion
  private def scalaReflect(scalacVersion: String): ModuleID = "org.scala-lang" % "scala-reflect" % scalacVersion
  private val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion
  private val sourcecode = "com.lihaoyi" %% "sourcecode" % sourcecodeVersion
  private val zio = "dev.zio" %% "zio" % zioVersion

  final object Plugins {

    val kindProjector = {
      compilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    }
  }

  final object CoreProject {

    def all(scalaVersion: String): Seq[ModuleID] =
      Seq(
        Plugins.kindProjector,
        alleyCatsCore,
        catsCore,
        catsEffect,
        catsFree,
        izumiReflect,
        scalactic,
        scalaReflect(scalaVersion),
        shapeless,
        sourcecode,
        zio, // used for Has[_] data type for now
      ) ++ Seq(
        // Test-only dependencies
        munit,
        munitCatsEffect,
        scalaCheck,
        scalaCheckOps,
        scalaTest,
        scalaTestPlusScalaCheck,
      ).map(_ % Test)
  }

  final object CirceV1Project {

    def all(scalaVersion: String): Seq[ModuleID] =
      CoreV1Project.all(scalaVersion) ++ Seq(
        circeCore,
        circeGeneric,
        circeParser,
      ) ++ Seq(
        // Test-only dependencies
        circeLiteral,
      ).map(_ % Test)
  }

}

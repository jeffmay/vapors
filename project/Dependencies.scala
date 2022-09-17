import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.8"

  private final val catsVersion = "2.8.0"
  private final val circeVersion = "0.14.2"
  private final val izumiReflectVersion = "2.2.0"
  private final val munitVersion = "1.0.0-M1" // it's okay to use milestone version for test code
  private final val scalacticVersion = "3.2.12"
  private final val scalaCheckVersion = "1.16.0"
  private final val scalaCheckOpsVersion = "2.9.0"
  private final val scalaTestVersion = "3.2.12"
  private final val scalaTestPlusScalaCheckVersion = "3.2.11.0"
  private final val shapelessVersion = "2.3.9"
  private final val sourcecodeVersion = "0.3.0"

  private val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  private val circeCore = "io.circe" %% "circe-core" % circeVersion
  private val circeLiteral = "io.circe" %% "circe-literal" % circeVersion
  private val circeGeneric = "io.circe" %% "circe-generic" % circeVersion
  private val circeParser = "io.circe" %% "circe-parser" % circeVersion
  private val izumiReflect = "dev.zio" %% "izumi-reflect" % izumiReflectVersion
  private val munit = "org.scalameta" %% "munit" % munitVersion
  private val munitScalaCheck = "org.scalameta" %% "munit-scalacheck" % munitVersion
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  private val scalaCheckOps = "com.rallyhealth" %% "scalacheck-ops_1-15" % scalaCheckOpsVersion
  private val scalactic = "org.scalactic" %% "scalactic" % scalacticVersion
  private val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  private val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalaTestPlusScalaCheckVersion
  private val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion
  private val sourcecode = "com.lihaoyi" %% "sourcecode" % sourcecodeVersion

  final object BenchProject {

    val all: Seq[ModuleID] = Seq(
      scalaCheckOps,
    ).map(_ % Test)
  }

  final object CoreV1Project {

    val all: Seq[ModuleID] =
      Seq(
        catsCore,
        izumiReflect,
        scalactic,
        shapeless,
        sourcecode,
      ) ++ Seq(
        // Test-only dependencies
        munit,
        munitScalaCheck,
        scalaCheck,
        scalaCheckOps,
        scalaTest,
        scalaTestPlusScalaCheck,
      ).map(_ % Test)
  }

  final object CirceV1Project {

    val all: Seq[ModuleID] =
      CoreV1Project.all ++ Seq(
        circeCore,
        circeGeneric,
        circeParser,
      ) ++ Seq(
        // Test-only dependencies
        circeLiteral,
      ).map(_ % Test)
  }

}

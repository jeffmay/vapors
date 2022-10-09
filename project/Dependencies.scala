import sbt._

object Dependencies {

  final val Scala_3 = "3.2.0"

  private final val catsVersion = "2.7.0"
  private final val izumiReflectVersion = "2.0.8"
  private final val munitVersion = "1.0.0-M1" // it's okay to use milestone version for test code
  private final val scalacticVersion = "3.2.10"
  private final val scalaCheckVersion = "1.15.4"
  private final val scalaCheckOpsVersion = "2.8.1"
  private final val scalaTestVersion = "3.2.10"
  private final val scalaTestPlusScalaCheckVersion = "3.2.10.0"
  private final val shapelessVersion = "3.0.4"
  private final val sourcecodeVersion = "0.2.7"

  private val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  private val izumiReflect = "dev.zio" %% "izumi-reflect" % izumiReflectVersion
  private val munit = "org.scalameta" %% "munit" % munitVersion
  private val munitScalaCheck = "org.scalameta" %% "munit-scalacheck" % munitVersion
  private val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  private val scalaCheckOps = "com.rallyhealth" %% "scalacheck-ops_1-15" % scalaCheckOpsVersion
  private val scalactic = "org.scalactic" %% "scalactic" % scalacticVersion
  private val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  private val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-15" % scalaTestPlusScalaCheckVersion
  private val shapeless = "org.typelevel" %% "shapeless3-deriving" % shapelessVersion
  private val sourcecode = "com.lihaoyi" %% "sourcecode" % sourcecodeVersion

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

}

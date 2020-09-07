import sbt._

object Dependencies {

  final val Scala_2_13 = "2.13.3"

  private final val catsVersion = "2.1.1"
  private final val circeVersion = "0.13.0"
  private final val kindProjectorVersion = "0.11.0"
  private final val scalaCheckVersion = "1.14.3"
  private final val scalaTestVersion = "3.2.2"
  private final val scalaTestPlusScalaCheckVersion = "3.2.2.0"

  val catsCore = "org.typelevel" %% "cats-core" % catsVersion
  val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  val circeCore = "io.circe" %% "circe-core" % circeVersion
  val circeParser = "io.circe" %% "circe-parser" % circeVersion
  val circeLiteral = "io.circe" %% "circe-literal" % circeVersion
  val scalaCheck = "org.scalacheck" %% "scalacheck" % scalaCheckVersion
  val scalaTest = "org.scalatest" %% "scalatest" % scalaTestVersion
  val scalaTestPlusScalaCheck = "org.scalatestplus" %% "scalacheck-1-14" % scalaTestPlusScalaCheckVersion
  def scalaReflect(scalacVersion: String): ModuleID = "org.scala-lang" % "scala-reflect" % scalacVersion

  object Plugins {
    val kindProjector = "org.typelevel" %% "kind-projector" % kindProjectorVersion
  }
}

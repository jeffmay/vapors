package com.rallyhealth.vapors.factfilter.evaluator

import cats.Order
import com.rallyhealth.vapors.factfilter.Example.{FactTypes, JoeSchmoe}
import com.rallyhealth.vapors.factfilter.data.{Evidence, FactTable, FactType}
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.wordspec.AnyWordSpec
import shapeless.HNil

class WrapOutputSpec extends AnyWordSpec {

  "Expr.WrapOutput" when {

    "compareing outputs" should {

      "wrap two constant integers and cast as a Point2D" in {
        val p = Point2D(1, 2)
        val q = {
          wrap(const(p.x), const(p.y)).as[Point2D]
        }
        val result = eval(FactTable.empty)(q)
        assertResult(p)(result.output.value)
      }

      "NOT compile when using three integers in a Point2D" in {
        assertDoesNotCompile {
          "wrap(const(1), const(2), const(3)).as[Point2D]"
        }
      }

      "convert const expressions into an expr of a tuple" in {
        val t = (1, "two", Color.Blue)
        val q = {
          wrap(const(t._1), const(t._2), const(t._3))
            .as[(Int, String, Color.Blue.type)]
        }
        val result = eval(FactTable.empty)(q)
        assertResult(t)(result.output.value)
      }

      "convert const expressions into an expr of a HList" in {
        val hlist = 1 :: "two" :: Color.Blue :: HNil
        val t = hlist.tupled
        val q = {
          wrap(const(t._1), const(t._2), const(t._3)).asHList
        }
        val result = eval(FactTable.empty)(q)
        assertResult(hlist)(result.output.value)
      }
    }

    "comparing evidence" should {

      "combine all non-empty evidence" in {
        val q = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val facts = List(JoeSchmoe.name, JoeSchmoe.age)
        val result = eval(FactTable(facts))(q)
        assertResult(Evidence(facts))(result.output.evidence)
      }

      "return no evidence if any branch has no evidence" in {
        val q = {
          wrap(
            factsOfType(FactTypes.Name).headOption,
            factsOfType(FactTypes.Age).headOption,
          ).asHList
        }
        val result = eval(FactTable(JoeSchmoe.name))(q)
        assertResult(Evidence.none)(result.output.evidence)
      }
    }
  }

}

// TODO: Use a fact type from the examples? Maybe an existing compound fact type?
final case class Point2D(
  x: Int,
  y: Int,
)

object Point2D {
  implicit val order: Order[Point2D] = Order.by(p => (p.x, p.y))
  val factType = FactType[Point2D]("point_2d")
}

sealed trait Color

object Color {
  final case object Red extends Color
  final case object Green extends Color
  final case object Blue extends Color
}

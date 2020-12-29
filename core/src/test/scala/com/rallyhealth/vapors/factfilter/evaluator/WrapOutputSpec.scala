package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.data.FactTable
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.wordspec.AnyWordSpec
import shapeless.{::, Generic, HNil}

class WrapOutputSpec extends AnyWordSpec {

  "wrap" should {

    "wrap two constant integers as a Point2D" in {
      val q = {
        wrap(const(1), const(2)).as[Point2D]
      }
      val result = eval(FactTable.empty)(q)
      assertResult(Point2D(1, 2))(result.output.value)
    }

    "NOT allow wrapping three integers as a Point2D" in {
      assertDoesNotCompile {
        "wrap(const(1), const(2), const(3)).as[Point2D]"
      }
    }
  }

}

final case class Point2D(
  x: Int,
  y: Int,
)

object Point2D {
  val gen: Generic.Aux[Point2D, Int :: Int :: HNil] = Generic[Point2D]
}

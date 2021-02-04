package com.rallyhealth.vapors.factfilter.dsl

import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.lens.DataPath
import com.rallyhealth.vapors.core.example.{FactTypes, GenericMeasurement, Probs}
import org.scalatest.Inside.inside
import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class ExprBuilderSpec extends AnyWordSpec {

  "ValExprBuilder" should {

    "combine lenses from chained .get() methods" in {
      val q = withFactsOfType(FactTypes.GenericMeasurement).where {
        _.exists {
          _.get(_.select(_.value)).get(_.select(_.value)) > 0.0
        }
      }
      inside(q) {
        case Expr.WithFactsOfType(_, Expr.ExistsInOutput(_, condExpr, _), _) =>
          inside(condExpr) {
            case Expr.OutputWithinWindow(Expr.SelectFromOutput(_, valueLens, _), _, _) =>
              assertResult(DataPath.empty.atField("value").atField("value")) {
                valueLens.path
              }
              val fact = FactTypes.GenericMeasurement(GenericMeasurement("example", 1.0, "m", Instant.now()))
              assertResult(fact.value.value) {
                valueLens.get(fact)
              }
          }
      }
    }

    "combine lenses from .get() and .getFoldable() methods" in {
      val q = withFactsOfType(FactTypes.ProbabilityToUse).where {
        _.exists {
          _.get(_.select(_.value)).getFoldable(_.select(_.scores)).isEmpty
        }
      }
      inside(q) {
        case Expr.WithFactsOfType(_, Expr.ExistsInOutput(_, condExpr, _), _) =>
          inside(condExpr) {
            case Expr.OutputIsEmpty(Expr.SelectFromOutput(_, valueLens, _), _) =>
              assertResult(DataPath.empty.atField("value").atField("scores")) {
                valueLens.path
              }
              val fact = FactTypes.ProbabilityToUse(Probs(Map()))
              assertResult(fact.value.scores) {
                valueLens.get(fact)
              }
          }
      }
    }
  }

}

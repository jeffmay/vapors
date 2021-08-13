package com.rallyhealth

package vapors.interpreter

import vapors.dsl._
import vapors.example.{FactTypes, User1}

import org.scalatest.freespec.AnyFreeSpec

class GroupOutputSpec extends AnyFreeSpec {

  "Expr.GroupOutput" - {

    "standard engine" - {
      allTests(StandardVaporsEngine)
    }

    "cats effect engine" - {
      import cats.effect.unsafe.implicits.global
      allTests(CatsEffectSimpleVaporsEngine)
    }
  }

  private def allTests[F[_]](
    engine: VaporsEngine[F, Unit],
  )(implicit
    engineExtractParam: engine.ExtractParam,
  ): Unit = {

    "create a map from a list using groupBy and mapValues" in {
      val query = valuesOfType(FactTypes.TagsUpdate).groupBy(_.select(_.source))
      val expected = User1.factTable.getSortedSeq(FactTypes.TagsUpdate).groupMap(_.value.source)(_.value)
      val result = engine.evalAndExtractValue(query, User1.factTable)
      assertResult(expected) {
        result.toMap
      }
    }
  }
}

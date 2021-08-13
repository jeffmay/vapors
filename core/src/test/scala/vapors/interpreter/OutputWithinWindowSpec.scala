package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class OutputWithinWindowSpec extends AnyFreeSpec {

  "Expr.OutputWithinWindow" - {

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

    "using the === operator" - {

      "return 'true' when the values are equal" in {
        val q = const(2 + 2) === const(4)
        val result = engine.evalAndExtractValue(q)
        assert(result)
      }

      "return 'false' when the values are not equal" in {
        val q = const(2 + 2) === const(5)
        val result = engine.evalAndExtractValue(q)
        assert(!result)
      }
    }

    "using the !== operator" - {

      "return 'false' when the values are equal" in {
        val q = const(2 + 2) !== const(4)
        val result = engine.evalAndExtractValue(q)
        assert(!result)
      }

      "return 'true' when the values are not equal" in {
        val q = const(2 + 2) !== const(5)
        val result = engine.evalAndExtractValue(q)
        assert(result)
      }
    }
  }
}

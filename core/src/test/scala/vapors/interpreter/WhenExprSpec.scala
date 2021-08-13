package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class WhenExprSpec extends AnyFreeSpec {

  // a set of ordered constants for comparison
  private final val a = "A"
  private final val b = "B"
  private final val c = "C"
  private final val d = "D"

  "Expr.When" - {

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

    "operating on constants" - {

      "return the first branch of an if / else when the first condition is 'true'" in {
        val q = when(const(true)).thenReturn(const(a)).elseReturn(const(b))
        val res = engine.evalAndExtractValue(q)
        assertResult(a)(res)
      }

      "return the second branch of an if / else when the first condition is 'false'" in {
        val q = when(const(false)).thenReturn(const(a)).elseReturn(const(b))
        val res = engine.evalAndExtractValue(q)
        assertResult(b)(res)
      }

      "return the first branch of a if / elif / else when the first condition is 'true' and the second condition is 'false'" in {
        val q = when(const(true))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = engine.evalAndExtractValue(q)
        assertResult(a)(res)
      }

      "return the first branch of a if / elif / else when the first condition is 'true' and the second condition is 'true'" in {
        val q = when(const(true))
          .thenReturn(const(a))
          .elif(const(true))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = engine.evalAndExtractValue(q)
        assertResult(a)(res)
      }

      "return the second branch of a if / elif / else when the first condition is 'false' and the second condition is 'true'" in {
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(true))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = engine.evalAndExtractValue(q)
        assertResult(b)(res)
      }

      "return the last branch of a if / elif / else when all conditions are 'false'" in {
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = engine.evalAndExtractValue(q)
        assertResult(c)(res)
      }

      "return the third branch of a if / elif / elif / else when the first two conditions are 'false' and the third condition is 'true'" in {
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elif(const(true))
          .thenReturn(const(c))
          .elseReturn(const(d))
        val res = engine.evalAndExtractValue(q)
        assertResult(c)(res)
      }
    }
  }
}

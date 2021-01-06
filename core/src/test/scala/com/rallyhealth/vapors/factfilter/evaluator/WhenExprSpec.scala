package com.rallyhealth.vapors.factfilter.evaluator

import com.rallyhealth.vapors.factfilter.data.FactTable
import com.rallyhealth.vapors.factfilter.dsl.ExprDsl._
import org.scalatest.wordspec.AnyWordSpec

class WhenExprSpec extends AnyWordSpec {

  "Expr.When" when {

    "operating on constants" should {

      "return the first branch of an if / else when the first condition is 'true'" in {
        val a = "A"
        val b = "B"
        val q = when(const(true)).thenReturn(const(a)).elseReturn(const(b))
        val res = eval(FactTable.empty)(q)
        assertResult(a)(res.output.value)
      }

      "return the second branch of an if / else when the first condition is 'false'" in {
        val a = "A"
        val b = "B"
        val q = when(const(false)).thenReturn(const(a)).elseReturn(const(b))
        val res = eval(FactTable.empty)(q)
        assertResult(b)(res.output.value)
      }

      "return the first branch of a if / elif / else when the first condition is 'true' and the second condition is 'false'" in {
        val a = "A"
        val b = "B"
        val c = "C"
        val q = when(const(true))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(a)(res.output.value)
      }

      "return the first branch of a if / elif / else when the first condition is 'true' and the second condition is 'true'" in {
        val a = "A"
        val b = "B"
        val c = "C"
        val q = when(const(true))
          .thenReturn(const(a))
          .elif(const(true))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(a)(res.output.value)
      }

      "return the second branch of a if / elif / else when the first condition is 'false' and the second condition is 'true'" in {
        val a = "A"
        val b = "B"
        val c = "C"
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(true))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(b)(res.output.value)
      }

      "return the last branch of a if / elif / else when all conditions are 'false'" in {
        val a = "A"
        val b = "B"
        val c = "C"
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elseReturn(const(c))
        val res = eval(FactTable.empty)(q)
        assertResult(c)(res.output.value)
      }

      "return the third branch of a if / elif / elif / else when the first two conditions are 'false' and the third condition is 'true'" in {
        val a = "A"
        val b = "B"
        val c = "C"
        val d = "D"
        val q = when(const(false))
          .thenReturn(const(a))
          .elif(const(false))
          .thenReturn(const(b))
          .elif(const(true))
          .thenReturn(const(c))
          .elseReturn(const(d))
        val res = eval(FactTable.empty)(q)
        assertResult(c)(res.output.value)
      }
    }
  }
}

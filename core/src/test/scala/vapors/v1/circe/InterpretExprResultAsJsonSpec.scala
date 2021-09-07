package com.rallyhealth

package vapors.v1.circe

import io.circe.literal._

import vapors.v1.algebra.Expr.debugExpr
import vapors.v1.debug.Debugging

import io.circe.syntax._

class InterpretExprResultAsJsonSpec extends munit.FunSuite {

  import vapors.v1.dsl.circe._

  test("serialize an expression state without input") {
    val expr = const(1) + const(2) + const(3)
    val result = expr.run()
    val output = result.state.output
    assertEquals(result.state.asJson, json"""{"output": $output}""")
  }

  test("serialize an expression state with input") {
    val expr = const(1) + const(2) + const(3)
    val input = "Hello"
    val result = expr.runWith(input)
    val output = result.state.output
    assertEquals(result.state.asJson, json"""{"input": $input, "output": $output}""")
  }

  test("serialize an expression result with input") {
    val expr = const(1) + const(2) + const(3)
    val result = expr.run()
    assertEquals(
      result.asJson,
      json"""{
        "expr": "combine",
        "operation": "add",
        "output": 6,
        "left": {
          "expr": "combine",
          "operation": "add",
          "output": 3,
          "left": {
            "expr": "const",
            "output": 1
          },
          "right": {
            "expr": "const",
            "output": 2
          }
        },
        "right": {
          "expr": "const",
          "output": 3
        }
      }""",
    )
  }

  test("should work with the debugger") {
    val x = (const(1) + const(3).debug(s => println(s.output)) + const(3)).debug { state =>
      val (i, a, b) = state.input
      println(s"i = $i")
      println(s"a = $a")
      println(s"b = $b")
    }
    x.run()
    val s = const(Seq(true, false)).exists(ident).debug { state =>
      val x = state.input
      println(x)
    }
    x.run()
  }

  test("serialize valuesOfType") {}

  test("Print the debugging") {
    println(Debugging[Any, Option[Int]](_ => {}))
  }
}

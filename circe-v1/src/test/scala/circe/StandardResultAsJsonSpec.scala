package com.rallyhealth.vapors.v1

package circe

import data.FactTable
import example.FactTypes

import io.circe.literal._
import io.circe.syntax._
import munit._

class StandardResultAsJsonSpec extends FunSuite {

  import dsl.circe.standard._

  test("serialize an expression state without input") {
    val expr = "World".const
    val result = expr.run()
    val output = result.state.output
    assertEquals(result.state.asJson, json"""{"output": $output}""")
  }

  test("serialize a state with input") {
    val expr = "World".const
    val input = "Hello"
    val result = expr.runWith(input)
    val output = result.state.output
    assertEquals(result.state.asJson, json"""{"input": $input, "output": $output}""")
  }

  test("serialize a nested combine result without input") {
    val expr = 1.const + 2.const + 3.const
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

  test("serialize an expression result with input") {
    val expr = 1.const + 2.const + 3.const
    val input = "Hello"
    val result = expr.runWith(input)
    assertEquals(
      result.asJson,
      json"""{
        "expr": "combine",
        "operation": "add",
        "input": $input,
        "output": 6,
        "left": {
          "expr": "combine",
          "operation": "add",
          "input": $input,
          "output": 3,
          "left": {
            "expr": "const",
            "input": $input,
            "output": 1
          },
          "right": {
            "expr": "const",
            "input": $input,
            "output": 2
          }
        },
        "right": {
          "expr": "const",
          "input": $input,
          "output": 3
        }
      }""",
    )
  }

  test("serialize valuesOfType with no input") {
    val ages = Seq(23, 42)
    val result = valuesOfType(FactTypes.Age).run(FactTable(ages.map(FactTypes.Age(_))))
    assertEquals(
      result.asJson,
      json"""{
        "expr": "valuesOfType",
        "factTypes": [${FactTypes.Age.name}],
        "output": $ages
      }""",
    )
  }
}

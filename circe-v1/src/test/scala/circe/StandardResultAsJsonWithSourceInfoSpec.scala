package com.rallyhealth.vapors.v1

package circe

import io.circe.literal._
import io.circe.syntax._
import munit.FunSuite

class StandardResultAsJsonWithSourceInfoSpec extends FunSuite {

  import dsl.circe.standard.withSourceInfo._

  private val thisFileName: String = implicitly[sourcecode.File].value
  private def thisFileAt(line: Int): String = s"$thisFileName:$line"

  test("serialize an expression result with debugging info") {
    val lineOfExpression = implicitly[sourcecode.Line].value + 1 // the expression is defined on the next line
    val result = 1.const.run()
    assertEquals(
      result.asJson,
      json"""{
        "source": ${thisFileAt(lineOfExpression)},
        "expr": "const",
        "output": 1
      }""",
    )
  }

  test("serialize an expression result with debugging info across multiple lines") {
    val lineOfFirstConst = implicitly[sourcecode.Line].value + 1 // the expression is defined on the next line
    val result = (1.const + // firstConst
      2.const).run() // secondConst
    val lineOfSecondConst = lineOfFirstConst + 1
    // unfortunately
    assertEquals(
      result.asJson.spaces2,
      json"""{
        "source": ${thisFileAt(lineOfFirstConst)},
        "expr": "combine",
        "output": 3,
        "operation": "add",
        "left": {
          "source": ${thisFileAt(lineOfFirstConst)},
          "expr": "const",
          "output": 1
        },
        "right": {
          "source": ${thisFileAt(lineOfSecondConst)},
          "expr": "const",
          "output": 2
        }  
      }""".spaces2,
    )
  }

  test("serialize the expression result of a combine holder when it is defined".fail) {
    val lineOfFirstConst = implicitly[sourcecode.Line].value + 1 // the expression is defined on the next line
    val expr = 1.const + // firstConst
      2.const // secondConst
    val result = expr.run() // unfortunately, this is where the combine expression param is collected
    // TODO: This could be fixed by using a different DebugCombineHolder that collects
    //       the source information when it is constructed and passes it to the
    //       HasSourceCodeInfo parameter collected later.
    val lineOfSecondConst = lineOfFirstConst + 1
    val lineOfExprRun = lineOfFirstConst + 2
    assertSuccess(lineOfExprRun) // this currently succeeds, but should fail
    assertSuccess(lineOfFirstConst) // this currently fails, but should succeed
    def assertSuccess(lineOfCombine: Int): Unit = {
      assertEquals(
        result.asJson.spaces2,
        json"""{
        "source": ${thisFileAt(lineOfCombine)},
        "expr": "combine",
        "output": 3,
        "operation": "add",
        "left": {
          "source": ${thisFileAt(lineOfFirstConst)},
          "expr": "const",
          "output": 1
        },
        "right": {
          "source": ${thisFileAt(lineOfSecondConst)},
          "expr": "const",
          "output": 2
        }  
      }""".spaces2,
      )
    }
  }
}

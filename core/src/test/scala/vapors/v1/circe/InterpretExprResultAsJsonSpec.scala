package com.rallyhealth

package vapors.v1.circe

import vapors.v1.data.FactTable

class InterpretExprResultAsJsonSpec extends munit.FunSuite {

  import vapors.v1.dsl._

  import com.rallyhealth.vapors.v1.dsl.circe._

  test("serialize an expression") {
    val x = const(1) + const(3) + const(3)
    val result = evalWithFactTable(x)(FactTable.empty)
    println(serialize(result))
  }
}

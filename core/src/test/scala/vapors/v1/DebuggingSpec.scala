package com.rallyhealth

package vapors.v1

import vapors.v1.data.ExprState
import vapors.v1.dsl.simple._

class DebuggingSpec extends munit.FunSuite {

  test("debug const") {

    const(1).debug { state =>
      assert(state.input == ExprState.Nothing)
      assert(state.output == 1)
    }
  }
}

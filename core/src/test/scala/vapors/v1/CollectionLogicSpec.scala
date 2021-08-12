package com.rallyhealth

package vapors.v1

import com.rallyhealth.vapors.v1.algebra.Expr
import com.rallyhealth.vapors.v1.dsl._
import munit.TestOptions

class CollectionLogicSpec extends munit.FunSuite {

  test("exists") {
    Expr.Exists()
  }
}

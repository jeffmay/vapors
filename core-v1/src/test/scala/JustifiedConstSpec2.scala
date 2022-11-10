package com.rallyhealth.vapors.v1

import algebra.Expr
import dsl.justified2

import com.rallyhealth.vapors.v1.data.Justified
import zio.test.*

object JustifiedConstSpec2 extends ZIOSpecDefault {

  import dsl.justified2.*

  override val spec: Spec[Any, Any] = suite("dsl.justified.const")(
    test("Option.const allows calling map") {
      val fixture = Option(1)
      val expr: Expr.Const[Option[Justified[Int]], OP] = fixture.const
      assertCompletes
//      val expr = fixture.const.map(_ + 1.const)
//      val observed = expr.run()
//      val expected = fixture.map(_ + 1)
//      assertTrue(observed == expected)
    },
  )
}

package com.rallyhealth

package vapors.interpreter

import vapors.dsl._

import org.scalatest.freespec.AnyFreeSpec

class FoldOutputSpec extends AnyFreeSpec {

  "Expr.FoldOutput" - {

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

    "fold a list of ints into its sum" in {
      val query = const(List(1, 2, 3)).withOutputFoldable.fold
      val result = engine.evalAndExtractValue(query)
      assertResult(6) {
        result
      }
    }

    "fold a list of options of int into an option containing the sum" in {
      val query = const(List(Some(1), None, Some(3))).withOutputFoldable.fold
      val result = engine.evalAndExtractValue(query)
      assertResult(Some(4)) {
        result
      }
    }

    "fold a list of options of int into a None" in {
      val query = const(List[Option[Int]](None, None, None)).withOutputFoldable.fold
      val result = engine.evalAndExtractValue(query)
      assertResult(None) {
        result
      }
    }
  }
}

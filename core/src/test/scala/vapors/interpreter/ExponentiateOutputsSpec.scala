package com.rallyhealth

package vapors.interpreter

import vapors.dsl
import vapors.dsl._

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec

import scala.reflect.classTag

class ExponentiateOutputsSpec extends AnyFreeSpec {

  "Expr.ExponentiateOutputs" - {

    "reasonable positive number raised to a reasonable positive exponent" in {
      VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
        Math.pow,
        (b, e) => dsl.pow(const(b), const(e)),
      )(Arbitrary(Gen.choose(0d, 100d)), Arbitrary(Gen.choose(1d, 10d)), classTag[ArithmeticException])
    }

    "arbitrary number raised to a arbitrary exponent" in {
      VaporsEvalTestHelpers.producesTheSameResultOrException[Double, Double, Double, ArithmeticException](
        Math.pow,
        (b, e) => dsl.pow(const(b), const(e)),
      )
    }
  }
}

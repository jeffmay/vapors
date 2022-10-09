package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq
import munit.FunSuite

class SimpleJustifiedWrapAllSpec extends FunSuite {

  import dsl.uncached.justified._

  // TODO: runWith() should not require justified input. There should be a WrapInput definition.

  test("wrapAll(None)") {
    val expr = wrapAll(None: Option[Any ~:> Int])
    val observed = expr.run()
    assertEquals(observed, None)
  }

  test("wrapAll(Some(expr)) with input") {
    val expr = wrapAll(Option((ident[Int] + 1.const).toExpr))
    val observed = expr.runWith(Justified.byConst(1))
    val expected = Some(
      Justified.byInference(
        "add",
        2,
        NonEmptySeq.of(
          Justified.byConst(1),
          Justified.byConst(1),
        ),
      ),
    )
    assertEquals(observed, expected)
  }

  test("wrapAll List of mixed input expressions") {
    val expr = wrapAll(List(1.const, (ident[Int] + 1.const).toExpr, (ident[Int] * 2.const).toExpr)).map(_ * 3.const)
    val input = 2
    val justifiedInput = Justified.byConst(input)
    val expected = List(
      Justified.byInference("multiply", 3, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(3))),
      Justified.byInference(
        "multiply",
        9,
        NonEmptySeq.of(
          Justified.byInference("add", 3, NonEmptySeq.of(justifiedInput, Justified.byConst(1))),
          Justified.byConst(3),
        ),
      ),
      Justified.byInference(
        "multiply",
        12,
        NonEmptySeq.of(
          Justified.byInference("multiply", 4, NonEmptySeq.of(justifiedInput, Justified.byConst(2))),
          Justified.byConst(3),
        ),
      ),
    )
    val observed = expr.runWith(justifiedInput)
    assertEquals(observed, expected)
  }

  test("wrapAll NonEmptySeq of mixed input expressions") {
    val expr =
      wrapAll(NonEmptySeq.of(1.const, (ident[Int] + 1.const).toExpr, (ident[Int] * 2.const).toExpr)).map(_ * 3.const)
    val input = 2
    val justifiedInput = Justified.byConst(input)
    val expected = NonEmptySeq.of(
      Justified.byInference("multiply", 3, NonEmptySeq.of(Justified.byConst(1), Justified.byConst(3))),
      Justified.byInference(
        "multiply",
        9,
        NonEmptySeq.of(
          Justified.byInference("add", 3, NonEmptySeq.of(justifiedInput, Justified.byConst(1))),
          Justified.byConst(3),
        ),
      ),
      Justified.byInference(
        "multiply",
        12,
        NonEmptySeq.of(
          Justified.byInference("multiply", 4, NonEmptySeq.of(justifiedInput, Justified.byConst(2))),
          Justified.byConst(3),
        ),
      ),
    )
    val observed = expr.runWith(justifiedInput)
    assertEquals(observed, expected)
  }
}

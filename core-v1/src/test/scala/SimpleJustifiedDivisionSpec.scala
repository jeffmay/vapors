package com.rallyhealth.vapors.v1

import data.Justified

import cats.data.NonEmptySeq
import munit.FunSuite

class SimpleJustifiedDivisionSpec extends FunSuite {

  import dsl.uncached.justified._

  test("Int / Int (exact)") {
    val expr = 2.const / 1.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        2,
        NonEmptySeq.of(Justified.byConst(2), Justified.byConst(1)),
      ),
    )
  }

  test("Int / Int (floor)") {
    val expr = 1.const / 2.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0,
        NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2)),
      ),
    )
  }

  test("Int / Int / Int (exact)") {
    val expr = 8.const / 2.const / 2.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        2,
        NonEmptySeq.of(
          Justified.byInference(
            "divide",
            4,
            NonEmptySeq.of(
              Justified.byConst(8),
              Justified.byConst(2),
            ),
          ),
          Justified.byConst(2),
        ),
      ),
    )
  }

  test("Long / Int (exact)") {
    val expr = 2L.const / 1.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        2L,
        NonEmptySeq.of(Justified.byConst(2L), Justified.byConst(1)),
      ),
    )
  }

  test("Long / Int (floor)") {
    val expr = 1L.const / 2.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0L,
        NonEmptySeq.of(Justified.byConst(1L), Justified.byConst(2)),
      ),
    )
  }

  test("Long / Int / Int (exact)") {
    val expr = 8L.const / 2.const / 2.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        2L,
        NonEmptySeq.of(
          Justified.byInference(
            "divide",
            4L,
            NonEmptySeq.of(
              Justified.byConst(8),
              Justified.byConst(2),
            ),
          ),
          Justified.byConst(2),
        ),
      ),
    )
  }

  test("Int / Long (exact)") {
    val expr = 2.const / 1L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        2L,
        NonEmptySeq.of(Justified.byConst(2), Justified.byConst(1L)),
      ),
    )
  }

  test("Int / Long (floor)") {
    val expr = 1.const / 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0L,
        NonEmptySeq.of(Justified.byConst(1), Justified.byConst(2L)),
      ),
    )
  }

  test("Int / Int / Long (exact)") {
    val expr = 8.const / 2.const / 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        2L,
        NonEmptySeq.of(
          Justified.byInference(
            "divide",
            4,
            NonEmptySeq.of(
              Justified.byConst(8),
              Justified.byConst(2),
            ),
          ),
          Justified.byConst(2L),
        ),
      ),
    )
  }

  test("Double / Int") {
    val expr = 2d.const / 4.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5d,
        NonEmptySeq.of(Justified.byConst(2d), Justified.byConst(4)),
      ),
    )
  }

  test("Double / Int / Long") {
    val expr = 4d.const / 4.const / 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5d,
        NonEmptySeq.of(
          Justified.byInference("divide", 1d, NonEmptySeq.of(Justified.byConst(4d), Justified.byConst(4))),
          Justified.byConst(2L),
        ),
      ),
    )
  }

  test("Int / Double") {
    val expr = 2.const / 4d.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5d,
        NonEmptySeq.of(Justified.byConst(2), Justified.byConst(4d)),
      ),
    )
  }

  test("Int / Double / Long") {
    val expr = 4.const / 4d.const / 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5d,
        NonEmptySeq.of(
          Justified.byInference("divide", 1d, NonEmptySeq.of(Justified.byConst(4), Justified.byConst(4d))),
          Justified.byConst(2L),
        ),
      ),
    )
  }

  test("Float / Int") {
    val expr = 2f.const / 4.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5f,
        NonEmptySeq.of(Justified.byConst(2f), Justified.byConst(4)),
      ),
    )
  }

  test("Float / Int / Long") {
    val expr = 4f.const / 4.const / 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5f,
        NonEmptySeq.of(
          Justified.byInference("divide", 1f, NonEmptySeq.of(Justified.byConst(4f), Justified.byConst(4))),
          Justified.byConst(2L),
        ),
      ),
    )
  }

  test("Int / Float") {
    val expr = 2.const / 4f.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5f,
        NonEmptySeq.of(Justified.byConst(2), Justified.byConst(4f)),
      ),
    )
  }

  test("Int / Float / Long") {
    val expr = 4.const / 4f.const / 2L.const
    val observed = expr.run()
    assertEquals(
      observed,
      Justified.byInference(
        "divide",
        0.5f,
        NonEmptySeq.of(
          Justified.byInference("divide", 1f, NonEmptySeq.of(Justified.byConst(4), Justified.byConst(4f))),
          Justified.byConst(2L),
        ),
      ),
    )
  }

}

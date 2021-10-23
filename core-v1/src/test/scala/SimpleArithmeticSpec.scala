package com.rallyhealth.vapors.v1

import java.time.{Duration, Instant}

class SimpleArithmeticSpec extends munit.FunSuite {

  import dsl.simple._

  test("Int + Int") {
    val expr = 1.const + 2.const
    val observed = expr.run()
    assertEquals(observed, 3)
  }

  test("Long + Int") {
    val expr = 1L.const + 2.const
    val observed = expr.run()
    assertEquals(observed, 3L)
  }

  test("Int + Long") {
    val expr = 1.const + 2L.const
    val observed = expr.run()
    assertEquals(observed, 3L)
  }

  test("Instant + Duration") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = now.const + duration.const
    val observed = expr.run()
    val expected = now.plus(duration)
    assertEquals(observed, expected)
  }

  test("Duration + Instant") {
    val now = Instant.now()
    val duration = Duration.ofMinutes(5)
    val expr = duration.const + now.const
    val observed = expr.run()
    val expected = now.plus(duration)
    assertEquals(observed, expected)
  }

  // TODO: LocalDate + Period
}

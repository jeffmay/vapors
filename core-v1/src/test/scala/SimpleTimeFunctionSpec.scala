package com.rallyhealth.vapors.v1

import testutil.TestClock

import munit.FunSuite

import java.time.{Instant, LocalDate}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

class SimpleTimeFunctionSpec extends FunSuite {

  import dsl.simple._

  test("now + 15.seconds.const produces a new Instant every time evaluated") {
    val clock = TestClock.oneSecondPerTickFromNow()
    val nowPlus15Sec = Instant.now(clock).plusSeconds(15)
    val expr = now(clock) + 15.seconds.const
    val firstTime = expr.run()
    assert(firstTime.isAfter(nowPlus15Sec))
    val secondTime = expr.run()
    assert(secondTime != firstTime)
    assert(secondTime.isAfter(firstTime))
  }

  test("today + 1.day.const produces a new LocalDate every time evaluated") {
    val clock = TestClock.oneUnitPerTickFromNow(TimeUnit.DAYS)
    // Add one to the day, because the next tick for "today" will be one day from today
    val dateToday = LocalDate.now(clock).plusDays(1)
    val dateTomorrow = dateToday.plusDays(1)
    val expr = today(clock) + 1.day.const
    val firstExpr = expr.run()
    assertEquals(firstExpr, dateTomorrow)
    val secondTime = expr.run()
    assertEquals(secondTime, dateTomorrow.plusDays(1))
  }
}

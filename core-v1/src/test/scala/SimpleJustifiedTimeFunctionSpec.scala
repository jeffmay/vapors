package com.rallyhealth.vapors.v1

import data.Justified
import testutil.TestClock

import cats.data.NonEmptySeq
import munit.FunSuite
import org.scalatest.Inside.inside

import java.time.{Instant, LocalDate}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

class SimpleJustifiedTimeFunctionSpec extends FunSuite {

  import dsl.simple.justified._

  test("now + 15.seconds.const produces a new Instant every time evaluated") {
    val clock = TestClock.oneSecondPerTickFromNow()
    val nowPlus15Sec = Instant.now(clock).plusSeconds(15)
    val expr = now(clock) + 15.seconds.const
    val firstTime = expr.run().value
    assert(firstTime.isAfter(nowPlus15Sec))
    val secondTime = expr.run()
    inside(secondTime) {
      case Justified.ByInference(reason, value, justified) =>
        assertNotEquals(value, firstTime)
        assert(value.isAfter(firstTime))
        assertEquals(reason, "add")
        assertEquals(
          justified,
          NonEmptySeq.of(
            Justified.ByConst(value.minusNanos(15.seconds.toNanos)),
            Justified.ByConst(15.seconds),
          ),
        )
    }
  }

  test("today + 1.day.const produces a new LocalDate every time evaluated") {
    val clock = TestClock.oneUnitPerTickFromNow(TimeUnit.DAYS)
    // Add one to the day, because the next tick for "today" will be one day from today
    val dateToday = LocalDate.now(clock).plusDays(1)
    val dateTomorrow = dateToday.plusDays(1)
    val expr = today(clock) + 1.day.const
    val firstTime = expr.run()
    assertEquals(
      firstTime,
      Justified.byInference(
        "add",
        dateTomorrow,
        NonEmptySeq.of(
          Justified.ByConst(dateToday),
          Justified.ByConst(1.day),
        ),
      ),
    )
    val secondTime = expr.run()
    assertEquals(
      secondTime,
      Justified.byInference(
        "add",
        dateTomorrow.plusDays(1),
        NonEmptySeq.of(
          Justified.ByConst(dateTomorrow),
          Justified.ByConst(1.day),
        ),
      ),
    )
  }
}

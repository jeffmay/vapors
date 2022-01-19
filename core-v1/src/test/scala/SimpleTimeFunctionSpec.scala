package com.rallyhealth.vapors.v1

import testutil.TestClock
import time.TemporalUnit

import munit.ScalaCheckSuite
import org.scalacheck.Prop

import java.time._
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

class SimpleTimeFunctionSpec extends ScalaCheckSuite {

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

  property("dateDiff of LocalDate in positive days") {
    Prop.forAll { (date: LocalDate) =>
      val date3DaysBefore = date.minusDays(3)
      val expr = dateDiff(date3DaysBefore.const, date.const, TemporalUnit.Days.const)
      assertEquals(expr.run(), 3L)
    }
  }

  property("dateDiff of LocalDate in negative days") {
    Prop.forAll { (date: LocalDate) =>
      val date3DaysAgo = date.minusDays(3)
      val expr = dateDiff(date.const, date3DaysAgo.const, TemporalUnit.Days.const)
      assertEquals(expr.run(), -3L)
    }
  }

  test("dateDiff of LocalDate in hours fails to compile") {
    val dateToday = LocalDate.now().const
    val message = compileErrors("dateDiff(dateToday, dateToday, TemporalUnit.Hours.const)")
    assert(
      message contains "Cannot count the number of com.rallyhealth.vapors.v1.time.TemporalUnit.Hours.type between two instances of java.time.LocalDate",
    )
  }

  property("dateDiff of LocalDate in positive millennia") {
    Prop.forAll { (date: LocalDate) =>
      val date1000YearsAgo = date.minusYears(1000)
      val expr = dateDiff(date1000YearsAgo.const, date.const, TemporalUnit.Millennia.const)
      assertEquals(expr.run(), 1L)
    }
  }

  property("dateDiff of LocalDate in negative millennia") {
    Prop.forAll { (date: LocalDate) =>
      val date1000YearsAgo = date.minusYears(1000)
      val expr = dateDiff(date.const, date1000YearsAgo.const, TemporalUnit.Millennia.const)
      assertEquals(expr.run(), -1L)
    }
  }

  property("dateDiff of LocalTime in positive nanos") {
    Prop.forAll { (time: LocalTime) =>
      val time3NanosAgo = time.minusNanos(3)
      val expr = dateDiff(time3NanosAgo.const, time.const, TemporalUnit.Nanos.const)
      assertEquals(expr.run(), 3L)
    }
  }

  property("dateDiff of LocalTime in negative nanos") {
    Prop.forAll { (time: LocalTime) =>
      val time3NanosAgo = time.minusNanos(3)
      val expr = dateDiff(time.const, time3NanosAgo.const, TemporalUnit.Nanos.const)
      assertEquals(expr.run(), -3L)
    }
  }

  property("dateDiff of LocalTime in positive hours") {
    Prop.forAll { (time: LocalTime) =>
      val time3HoursAgo = time.minusHours(3)
      val expr = dateDiff(time3HoursAgo.const, time.const, TemporalUnit.Hours.const)
      // LocalTime wraps around near the start of the day, so we have to change our expectation
      val expected = if (time3HoursAgo.isAfter(time)) 3 - 24 else 3L
      assertEquals(expr.run(), expected)
    }
  }

  property("dateDiff of LocalTime in negative hours") {
    Prop.forAll { (time: LocalTime) =>
      val time3HoursAgo = time.minusHours(3)
      val expr = dateDiff(time.const, time3HoursAgo.const, TemporalUnit.Hours.const)
      // LocalTime wraps around near the end of the day, so we have to change our expectation
      val expected = if (time3HoursAgo.isAfter(time)) -3 + 24 else -3L
      assertEquals(expr.run(), expected)
    }
  }

  test("dateDiff of LocalTime in days fails to compile") {
    val dateToday = LocalTime.now().const
    val message = compileErrors("dateDiff(dateToday, dateToday, TemporalUnit.Days.const)")
    assert(
      message contains "Cannot count the number of com.rallyhealth.vapors.v1.time.TemporalUnit.Days.type between two instances of java.time.LocalTime",
    )
  }

  property("dateDiff of LocalDateTime in positive nanos") {
    Prop.forAll { (dateTime: LocalDateTime) =>
      val time3NanosAgo = dateTime.minusNanos(3)
      val expr = dateDiff(time3NanosAgo.const, dateTime.const, TemporalUnit.Nanos.const)
      assertEquals(expr.run(), 3L)
    }
  }

  property("dateDiff of LocalDateTime in negative nanos") {
    Prop.forAll { (dateTime: LocalDateTime) =>
      val time3NanosAgo = dateTime.minusNanos(3)
      val expr = dateDiff(dateTime.const, time3NanosAgo.const, TemporalUnit.Nanos.const)
      assertEquals(expr.run(), -3L)
    }
  }

  property("dateDiff of LocalDateTime in positive millennia") {
    Prop.forAll { (dateTime: LocalDateTime) =>
      val date1000YearsAgo = dateTime.minusYears(1000)
      val expr = dateDiff(date1000YearsAgo.const, dateTime.const, TemporalUnit.Millennia.const)
      assertEquals(expr.run(), 1L)
    }
  }

  property("dateDiff of LocalDateTime in negative millennia") {
    Prop.forAll { (dateTime: LocalDateTime) =>
      val date1000YearsAgo = dateTime.minusYears(1000)
      val expr = dateDiff(dateTime.const, date1000YearsAgo.const, TemporalUnit.Millennia.const)
      assertEquals(expr.run(), -1L)
    }
  }

  property("dateDiff of ZonedDateTime in positive nanos") {
    Prop.forAll { (dateTime: ZonedDateTime) =>
      val time3NanosAgo = dateTime.minusNanos(3)
      val expr = dateDiff(time3NanosAgo.const, dateTime.const, TemporalUnit.Nanos.const)
      assertEquals(expr.run(), 3L)
    }
  }

  property("dateDiff of ZonedDateTime in negative nanos") {
    Prop.forAll { (dateTime: ZonedDateTime) =>
      val time3NanosAgo = dateTime.minusNanos(3)
      val expr = dateDiff(dateTime.const, time3NanosAgo.const, TemporalUnit.Nanos.const)
      assertEquals(expr.run(), -3L)
    }
  }

  property("dateDiff of ZonedDateTime in positive millennia") {
    Prop.forAll { (dateTime: ZonedDateTime) =>
      val date1000YearsAgo = dateTime.minusYears(1000)
      val expr = dateDiff(date1000YearsAgo.const, dateTime.const, TemporalUnit.Millennia.const)
      assertEquals(expr.run(), 1L)
    }
  }

  property("dateDiff of ZonedDateTime in negative millennia") {
    Prop.forAll { (dateTime: ZonedDateTime) =>
      val date1000YearsAgo = dateTime.minusYears(1000)
      val expr = dateDiff(dateTime.const, date1000YearsAgo.const, TemporalUnit.Millennia.const)
      assertEquals(expr.run(), -1L)
    }
  }
}

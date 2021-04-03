package com.rallyhealth.vapors.core.interpreter

import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.FactTable
import com.rallyhealth.vapors.core.dsl._
import com.rallyhealth.vapors.core.example.{FactTypes, Snippets}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.ops._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._

import java.time.temporal.{ChronoUnit, Temporal, TemporalAmount}
import java.time._
import scala.util.Try

class TimeFunctionsSpec extends AnyFreeSpec {
  import TimeFunctionsSpec._

  private def testProducesTheSameResult[T <: Temporal : Arbitrary, D <: TemporalAmount : Arbitrary](
    evaluateDirectly: (T, D) => T,
    buildQuery: (RootExpr[T, Unit], RootExpr[D, Unit]) => RootExpr[T, Unit],
  ): Assertion = {
    forAll { (temporal: T, amount: D) =>
      val query = buildQuery(const(temporal), const(amount))
      Try(evaluateDirectly(temporal, amount)).fold(
        expectedErr => {
          val err = intercept[DateTimeException] {
            eval(FactTable.empty)(query)
          }
          assertResult(expectedErr.getMessage) {
            err.getMessage
          }
        },
        expectedValue => {
          val result = eval(FactTable.empty)(query)
          assertResult(expectedValue) {
            result.output.value
          }
        },
      )
    }
  }

  "dateAdd" - {

    "Instant with" - {

      "Duration works the same as .plus" in {
        testProducesTheSameResult[Instant, Duration](
          _.plus(_),
          dateAdd(_, _),
        )
      }

      "Period fails to compile" in {
        val now = Instant.now()
        val period = Period.ofYears(1)
        assertDoesNotCompile {
          "dateAdd(const(now), const(period))"
        }
        assertThrows[DateTimeException] {
          now.plus(period)
        }
      }
    }

    "LocalDate with" - {

      "Duration fails to compile" in {
        val today = LocalDate.now()
        val duration = Duration.ofSeconds(10)
        assertDoesNotCompile {
          "dateAdd(const(today), const(duration))"
        }
        assertThrows[DateTimeException] {
          today.plus(duration)
        }
      }

      "Period works the same as .plus" in {
        testProducesTheSameResult[LocalDate, Period](
          _.plus(_),
          dateAdd(_, _),
        )
      }
    }

    "LocalDateTime with" - {

      "Duration works the same as .plus" in {
        testProducesTheSameResult[LocalDateTime, Duration](
          _.plus(_),
          dateAdd(_, _),
        )
      }

      "Period works the same as .plus" in {
        testProducesTheSameResult[LocalDateTime, Period](
          _.plus(_),
          dateAdd(_, _),
        )
      }
    }

    "ZonedDateTime with" - {

      "Duration works the same as .plus" in {
        testProducesTheSameResult[ZonedDateTime, Duration](
          _.plus(_),
          dateAdd(_, _),
        )
      }

      "Period works the same as .plus" in {
        testProducesTheSameResult[ZonedDateTime, Period](
          _.plus(_),
          dateAdd(_, _),
        )
      }
    }
  }

  "dateDiff" - {

    "computing Age from DateOfBirth" - {

      val feb28Year2021 = LocalDate.of(2021, 2, 28)
      val onFeb28Year2021 = new Snippets(feb28Year2021)

      // leap year
      val feb29Year2020 = LocalDate.of(2020, 2, 29)
      val onFeb29Year2020 = new Snippets(feb29Year2020)

      "rounds the year down" in {
        val feb1Minus20Years = LocalDate.of(feb28Year2021.getYear - 20, 2, 1)
        // validate that it works outside of vapors
        assertResult(20) {
          feb1Minus20Years.until(feb28Year2021, ChronoUnit.YEARS)
        }
        // validate that it works inside of vapors
        val result = eval(FactTable(FactTypes.DateOfBirth(feb1Minus20Years))) {
          onFeb28Year2021.ageFromDateOfBirth
        }
        assertResult(Seq(20)) { // Just turned 20 this month
          result.output.value
        }
      }

      "rounds the year down, even when close" in {
        val feb29Year2000 = feb28Year2021.minusYears(21).plusDays(1)
        // validate that it works outside of vapors
        assertResult(20) {
          feb29Year2000.until(feb28Year2021, ChronoUnit.YEARS)
        }
        // validate that it works inside of vapors
        val result = eval(FactTable(FactTypes.DateOfBirth(feb29Year2000))) {
          onFeb28Year2021.ageFromDateOfBirth
        }
        assertResult(Seq(20)) { // Sorry, birthday is tomorrow
          result.output.value
        }
      }

      "counts the current year on their birthday" in {
        val feb28Year2000 = feb28Year2021.minusYears(21)
        // validate that it works outside of vapors
        assertResult(21) {
          feb28Year2000.until(feb28Year2021, ChronoUnit.YEARS)
        }
        // validate that it works inside of vapors
        val result = eval(FactTable(FactTypes.DateOfBirth(feb28Year2000))) {
          onFeb28Year2021.ageFromDateOfBirth
        }
        assertResult(Seq(21)) { // 🎉 happy birthday! 🍻
          result.output.value
        }
      }

      "returns the correct number of years between leap years" in {
        val feb29Year2000 = LocalDate.of(2000, 2, 29)
        // validate that it works outside of vapors
        assertResult(20) {
          feb29Year2000.until(feb29Year2020, ChronoUnit.YEARS)
        }
        // validate that it works inside of vapors
        val result = eval(FactTable(FactTypes.DateOfBirth(feb29Year2000))) {
          new Snippets(feb29Year2020).ageFromDateOfBirth
        }
        assertResult(Seq(20)) { // leap years work as expected
          result.output.value
        }
      }

      "returns the correct number of years on a leap year" in {
        val mar1Year1999 = LocalDate.of(1999, 3, 1)
        // validate that it works outside of vapors
        assertResult(20) {
          mar1Year1999.until(feb29Year2020, ChronoUnit.YEARS)
        }
        // validate that it works inside of vapors
        val result = eval(FactTable(FactTypes.DateOfBirth(mar1Year1999))) {
          onFeb29Year2020.ageFromDateOfBirth
        }
        assertResult(Seq(20)) {
          result.output.value
        }
      }

      "returns the correct number of years from a leap year birthday" in {
        val feb29Year2000 = LocalDate.of(2000, 2, 29)
        // validate that it works outside of vapors
        assertResult(20) {
          feb29Year2000.until(feb28Year2021, ChronoUnit.YEARS)
        }
        // validate that it works inside of vapors
        val result = eval(FactTable(FactTypes.DateOfBirth(feb29Year2000))) {
          onFeb28Year2021.ageFromDateOfBirth
        }
        assertResult(Seq(20)) {
          result.output.value
        }
      }
    }
  }
}

object TimeFunctionsSpec {

  // TODO: Add to scalacheck-ops

  // $COVERAGE-OFF$
  implicit val arbChronoUnit: Arbitrary[ChronoUnit] = Arbitrary {
    Gen.oneOf(ChronoUnit.values())
  }

  implicit val arbJDuration: Arbitrary[Duration] = Arbitrary {
    for {
      a <- arbitrary[Instant]
      b <- arbitrary[Instant]
    } yield Duration.between(a, b)
  }

  implicit val arbJPeriod: Arbitrary[Period] = Arbitrary {
    for {
      a <- arbitrary[LocalDate]
      b <- arbitrary[LocalDate]
    } yield Period.between(a, b)
  }
  // $COVERAGE-ON$
}

package com.rallyhealth

package vapors.interpreter

import vapors.data.FactTable
import vapors.dsl._
import vapors.example.{FactTypes, Snippets}

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.ops._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec

import java.time._
import java.time.temporal.ChronoUnit

class TimeFunctionsSpec extends AnyFreeSpec {

  import TimeFunctionsSpec._

  "datetime functions" - {

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

    "dateAdd" - {

      "Instant with" - {

        "Duration works the same as .plus" in {
          VaporsEvalTestHelpers.producesTheSameResultOrException[F, Instant, Duration, Instant, DateTimeException](
            _.plus(_),
            (t, d) => dateAdd(const(t), const(d)),
            engine,
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
          VaporsEvalTestHelpers.producesTheSameResultOrException[F, LocalDate, Period, LocalDate, DateTimeException](
            _.plus(_),
            (t, d) => dateAdd(const(t), const(d)),
            engine,
          )
        }
      }

      "LocalDateTime with" - {

        "Duration works the same as .plus" in {
          VaporsEvalTestHelpers
            .producesTheSameResultOrException[F, LocalDateTime, Duration, LocalDateTime, DateTimeException](
              _.plus(_),
              (t, d) => dateAdd(const(t), const(d)),
              engine,
            )
        }

        "Period works the same as .plus" in {
          VaporsEvalTestHelpers
            .producesTheSameResultOrException[F, LocalDateTime, Period, LocalDateTime, DateTimeException](
              _.plus(_),
              (t, d) => dateAdd(const(t), const(d)),
              engine,
            )
        }
      }

      "ZonedDateTime with" - {

        "Duration works the same as .plus" in {
          VaporsEvalTestHelpers
            .producesTheSameResultOrException[F, ZonedDateTime, Duration, ZonedDateTime, DateTimeException](
              _.plus(_),
              (t, d) => dateAdd(const(t), const(d)),
              engine,
            )
        }

        "Period works the same as .plus" in {
          VaporsEvalTestHelpers
            .producesTheSameResultOrException[F, ZonedDateTime, Period, ZonedDateTime, DateTimeException](
              _.plus(_),
              (t, d) => dateAdd(const(t), const(d)),
              engine,
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
          val result = engine.evalAndExtractValue(
            onFeb28Year2021.ageFromDateOfBirth,
            FactTable(FactTypes.DateOfBirth(feb1Minus20Years)),
          )
          assertResult(Seq(20)) { // Just turned 20 this month
            result
          }
        }

        "rounds the year down, even when close" in {
          val feb29Year2000 = feb28Year2021.minusYears(21).plusDays(1)
          // validate that it works outside of vapors
          assertResult(20) {
            feb29Year2000.until(feb28Year2021, ChronoUnit.YEARS)
          }
          // validate that it works inside of vapors
          val result = engine.evalAndExtractValue(
            onFeb28Year2021.ageFromDateOfBirth,
            FactTable(FactTypes.DateOfBirth(feb29Year2000)),
          )
          assertResult(Seq(20)) { // Sorry, birthday is tomorrow
            result
          }
        }

        "counts the current year on their birthday" in {
          val feb28Year2000 = feb28Year2021.minusYears(21)
          // validate that it works outside of vapors
          assertResult(21) {
            feb28Year2000.until(feb28Year2021, ChronoUnit.YEARS)
          }
          // validate that it works inside of vapors
          val result = engine.evalAndExtractValue(
            onFeb28Year2021.ageFromDateOfBirth,
            FactTable(FactTypes.DateOfBirth(feb28Year2000)),
          )
          assertResult(Seq(21)) { // 🎉 happy birthday! 🍻
            result
          }
        }

        "returns the correct number of years between leap years" in {
          val feb29Year2000 = LocalDate.of(2000, 2, 29)
          // validate that it works outside of vapors
          assertResult(20) {
            feb29Year2000.until(feb29Year2020, ChronoUnit.YEARS)
          }
          // validate that it works inside of vapors
          val result = engine.evalAndExtractValue(
            new Snippets(feb29Year2020).ageFromDateOfBirth,
            FactTable(FactTypes.DateOfBirth(feb29Year2000)),
          )
          assertResult(Seq(20)) { // leap years work as expected
            result
          }
        }

        "returns the correct number of years on a leap year" in {
          val mar1Year1999 = LocalDate.of(1999, 3, 1)
          // validate that it works outside of vapors
          assertResult(20) {
            mar1Year1999.until(feb29Year2020, ChronoUnit.YEARS)
          }
          // validate that it works inside of vapors
          val result =
            engine.evalAndExtractValue(
              onFeb29Year2020.ageFromDateOfBirth,
              FactTable(FactTypes.DateOfBirth(mar1Year1999)),
            )
          assertResult(Seq(20)) {
            result
          }
        }

        "returns the correct number of years from a leap year birthday" in {
          val feb29Year2000 = LocalDate.of(2000, 2, 29)
          // validate that it works outside of vapors
          assertResult(20) {
            feb29Year2000.until(feb28Year2021, ChronoUnit.YEARS)
          }
          // validate that it works inside of vapors
          val result = engine.evalAndExtractValue(
            onFeb28Year2021.ageFromDateOfBirth,
            FactTable(FactTypes.DateOfBirth(feb29Year2000)),
          )
          assertResult(Seq(20)) {
            result
          }
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

package com.rallyhealth.vapors.core.example

import cats.Id
import com.rallyhealth.vapors.core.algebra.Expr
import com.rallyhealth.vapors.core.data.{FactTable, FactType}
import com.rallyhealth.vapors.core.dsl._

import java.time.temporal.ChronoUnit
import java.time._

class Snippets(val clock: Clock) {

  def this(fixedInstant: Instant) = this(Clock.fixed(fixedInstant, ZoneOffset.UTC))

  def this(fixedLocalDate: LocalDate) =
    this(Clock.fixed(fixedLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant, ZoneId.systemDefault()))

  val ageFromDateOfBirth: Expr[FactTable, Seq[Int], Unit] = {
    valuesOfType(FactTypes.DateOfBirth).map { value =>
      val ageInYears = dateDiff(
        value,
        value.embedResult(today(clock)),
        value.embedConst(ChronoUnit.YEARS),
      )
      ageInYears.withOutputValue.get(_.select(_.toInt))
    }
  }

  val ageFromDateOfBirthDef: Expr.Definition[Unit] = {
    define(FactTypes.Age).fromEvery {
      ageFromDateOfBirth
    }
  }

  val isOver18: RootExpr[Boolean, Unit] = {
    usingDefinitions(ageFromDateOfBirthDef) {
      valuesOfType(FactTypes.Age).exists {
        _ >= 18
      }
    }
  }

  lazy val isUser: RootExpr[Boolean, Unit] = {
    valuesOfType(FactTypes.Role).exists {
      _ >= Role.User
    }
  }

  val isEligible: RootExpr[Boolean, Unit] = and(isOver18, isUser)

  val isEligibleDef: Expr.Define[Id, Boolean, Unit] =
    define(FactType[Boolean]("is_eligible")).from(isEligible)
}

object Snippets extends Snippets(Clock.systemDefaultZone())

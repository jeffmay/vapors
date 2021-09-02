package com.rallyhealth

package vapors.example

import vapors.algebra.Expr
import vapors.data.{FactTable, FactType}
import vapors.dsl._

import cats.Id

import java.time._
import java.time.temporal.ChronoUnit

class Snippets(val clock: Clock) {

  def this(fixedInstant: Instant) = this(Clock.fixed(fixedInstant, ZoneOffset.UTC))

  def this(fixedLocalDate: LocalDate) =
    this(Clock.fixed(fixedLocalDate.atStartOfDay(ZoneId.systemDefault()).toInstant, ZoneId.systemDefault()))

  val ageFromDateOfBirth: Expr[FactTable, Seq[Int], Unit] = {
    valuesOfType(FactTypes.DateOfBirth).map { dob =>
      val ageInYears = dateDiff(
        dob,
        dob.embedResult(today(clock)),
        dob.embedConst(ChronoUnit.YEARS),
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
      factsOfType(FactTypes.Age).exists {
        _.value >= const(18)
      }
    }
  }

  lazy val isUser: RootExpr[Boolean, Unit] = {
    factsOfType(FactTypes.Role).exists {
      _.value >= const(Role.User: Role)
    }
  }

  val isEligible: RootExpr[Boolean, Unit] = and(isOver18, isUser)

  val isEligibleDef: Expr.Define[Id, Boolean, Unit] =
    define(FactType[Boolean]("is_eligible")).from(isEligible)
}

object Snippets extends Snippets(Clock.systemDefaultZone())

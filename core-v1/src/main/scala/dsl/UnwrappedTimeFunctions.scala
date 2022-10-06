package com.rallyhealth.vapors.v1

package dsl
import time.CountTime

import java.time.{Clock, Instant, LocalDate}

trait UnwrappedTimeFunctions extends TimeFunctions with UnwrappedDslTypes {

  override final def dateDiff[I, T, U](
    lhsExpr: I ~:> T,
    rhsExpr: I ~:> T,
    roundToUnitExpr: I ~:> U,
  )(implicit
    countTime: CountTime[T, U, Long],
    opL: OP[T *: T *: U *: EmptyTuple],
    opO: OP[Long],
  ): AndThen[I, T *: T *: U *: EmptyTuple, Long] =
    super.dateDiff(lhsExpr, rhsExpr, roundToUnitExpr)(countTime, opL, opO)

  override final def today(
    implicit
    opC: OP[LocalDate],
    opO: OP[LocalDate],
  ): Any ~:> LocalDate = super.today(opC, opO)

  override final def today(
    clock: Clock,
  )(implicit
    opC: OP[LocalDate],
    opO: OP[LocalDate],
  ): Any ~:> LocalDate = super.today(clock)(opC, opO)

  override final def now(
    implicit
    opC: OP[Instant],
    opO: OP[Instant],
  ): Any ~:> Instant = super.now(opC, opO)

  override final def now(
    clock: Clock,
  )(implicit
    opC: OP[Instant],
    opO: OP[Instant],
  ): Any ~:> Instant = super.now(clock)(opC, opO)
}

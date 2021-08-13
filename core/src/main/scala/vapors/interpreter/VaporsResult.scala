package com.rallyhealth

package vapors.interpreter

import vapors.algebra.ExprResult
import vapors.data.{Evidence, FactTable}

import cats.Id
import cats.effect.IO

sealed trait VaporsResult[F[_], R, P] {

  def value: F[R]

  def maybeEvidence: Option[F[Evidence]]

  def maybeParam: Option[F[P]]
}

final case class StandardVaporsResult[R, P](result: ExprResult[FactTable, R, P]) extends VaporsResult[Id, R, P] {
  override def value: R = result.output.value
  override def maybeEvidence: Option[Evidence] = Some(result.output.evidence)
  override lazy val maybeParam: Option[P] = Some(result.param.value)
}

final case class CatsEffectSimpleVaporsResult[R](value: IO[R]) extends VaporsResult[IO, R, Unit] {
  override def maybeEvidence: Option[IO[Evidence]] = None
  override def maybeParam: Option[IO[Unit]] = None
}

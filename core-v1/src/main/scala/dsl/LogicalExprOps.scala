package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import logic.{Conjunction, Disjunction, Negation}

import cats.data.NonEmptyVector

final class LogicalExprOps[I, B, F[+_], OP[_]](private val expr: Expr[I, F[B], OP]) extends AnyVal {

  def &&[CI <: I, CB >: B](
    that: Expr[CI, F[CB], OP],
  )(implicit
    logic: Conjunction[F, CB, OP],
    opB: OP[F[CB]],
  ): Expr.And[CI, CB, F, OP] =
    Expr.And(expr, NonEmptyVector.one(that))

  def ||[CI <: I, CB >: B](
    that: Expr[CI, F[CB], OP],
  )(implicit
    logic: Disjunction[F, CB, OP],
    opB: OP[F[CB]],
  ): Expr.Or[CI, CB, F, OP] =
    Expr.Or(expr, NonEmptyVector.one(that))

  def unary_!(
    implicit
    logic: Negation[F, B, OP],
    opB: OP[F[B]],
  ): Expr.Not[I, B, F, OP] =
    Expr.Not(expr)
}

package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import logic.{Conjunction, Disjunction, Negation}

import cats.data.NonEmptyVector

final class LogicalExprOps[I, B, W[+_], OP[_]](private val expr: Expr[I, W[B], OP]) extends AnyVal {

  def &&[CI <: I, CB >: B](
    that: Expr[CI, W[CB], OP],
  )(implicit
    logic: Conjunction[W, CB, OP],
    opB: OP[W[CB]],
  ): Expr.And[CI, CB, W, OP] =
    Expr.And(expr, NonEmptyVector.one(that))

  def ||[CI <: I, CB >: B](
    that: Expr[CI, W[CB], OP],
  )(implicit
    logic: Disjunction[W, CB, OP],
    opB: OP[W[CB]],
  ): Expr.Or[CI, CB, W, OP] =
    Expr.Or(expr, NonEmptyVector.one(that))

  def unary_!(
    implicit
    logic: Negation[W, B, OP],
    opB: OP[W[B]],
  ): Expr.Not[I, B, W, OP] =
    Expr.Not(expr)
}

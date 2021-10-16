package com.rallyhealth.vapors.v1

package engine

import algebra.Expr

import cats.data.NonEmptyList
import cats.{Eval, Foldable}

/**
  * Shared implementation for expression interpreters.
  *
  * @tparam OP the type of output parameter. This is unused, but must be defined to satisfy the compiler.
  */
trait CommonEngine[OP[_]] {
  import cats.implicits._

  protected def visitForAllCommon[C[_] : Foldable, A, B : OP](
    expr: Expr.ForAll[C, A, B, OP],
    ca: C[A],
  )(
    applyCondition: A => B,
  ): B = {
    val falseOrTrueResults = ca.foldRight[Either[NonEmptyList[B], List[B]]](Eval.now(Right(Nil))) { (a, evalResults) =>
      val b = applyCondition(a)
      val isTrue = expr.asBoolean(b)
      if (isTrue) {
        evalResults.map {
          case Right(ts) => Right(b :: ts) // combine true evidence for true result
          case bs => bs // exclude true evidence for false result
        }
      } else {
        if (expr.shortCircuit) Eval.now(Left(NonEmptyList.of(b)))
        else {
          evalResults.map {
            case Right(_) => Left(NonEmptyList.of(b)) // this whole expression is now a false result
            case Left(fs) => Left(b :: fs) // combine false evidence for false result
          }
        }
      }
    }
    falseOrTrueResults.value.fold(expr.combineFalse, expr.combineTrue)
  }

  protected def visitExistsCommon[C[_] : Foldable, A, B : OP](
    expr: Expr.Exists[C, A, B, OP],
    ca: C[A],
  )(
    applyCondition: A => B,
  ): B = {
    val falseOrTrueResults = ca.foldRight[Either[List[B], NonEmptyList[B]]](Eval.now(Left(Nil))) { (a, evalResults) =>
      val b = applyCondition(a)
      val isTrue = expr.asBoolean(b)
      if (isTrue) {
        if (expr.shortCircuit) Eval.now(Right(NonEmptyList.of(b)))
        else
          evalResults.map {
            case Left(_) => Right(NonEmptyList.of(b)) // this whole expression is now a true result
            case Right(ts) => Right(b :: ts) // combine true evidence for true result
          }
      } else
        evalResults.map {
          case Left(fs) => Left(b :: fs) // combine false evidence for false result
          case bs => bs // exclude false evidence for true result
        }
    }
    falseOrTrueResults.value.fold(expr.combineFalse, expr.combineTrue)
  }
}

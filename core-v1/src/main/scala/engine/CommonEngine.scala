package com.rallyhealth.vapors.v1

package engine

import algebra.Expr
import data.ExtractValue

import cats.data.NonEmptyList
import cats.{Eval, Foldable}

/**
  * Shared implementation for expression interpreters.
  *
  * @tparam OP the type of output parameter. This is unused, but must be defined to satisfy the compiler.
  */
trait CommonEngine[OP[_]] {
  import cats.implicits._

  protected def visitForAllCommon[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
    expr: Expr.ForAll[C, A, B, OP],
    ca: C[A],
  )(
    applyCondition: A => B,
  ): (Either[NonEmptyList[B], List[B]], B) = {
    val falseOrTrueResults = ca
      .foldRight[Either[NonEmptyList[B], List[B]]](Eval.now(Right(Nil))) { (a, evalResults) =>
        val b = applyCondition(a)
        val isTrue = ExtractValue.asBoolean(b)
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
      .value
    val o = falseOrTrueResults.fold(expr.combineFalse, expr.combineTrue)
    (falseOrTrueResults, o)
  }

  // TODO: How to support caching between values in here?
  protected def visitExistsCommon[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, S](
    expr: Expr.Exists[C, A, B, OP],
    ca: C[A],
    init: S,
  )(
    applyCondition: (A, S) => (B, S),
  ): (Either[List[B], NonEmptyList[B]], B, S) = {
    //    val falseOrTrueResults = ca
    //      .foldRight[Either[List[B], NonEmptyList[B]]](Eval.now(Left(Nil))) { (a, evalResults) =>
    //        val b = applyCondition(a, lastState)
    //        val isTrue = ExtractValue.asBoolean(b)
    //        if (isTrue) {
    //          if (expr.shortCircuit) Eval.now(Right(NonEmptyList.of(b)))
    //          else
    //            evalResults.map {
    //              case Left(_) => Right(NonEmptyList.of(b)) // this whole expression is now a true result
    //              case Right(ts) => Right(b :: ts) // combine true evidence for true result
    //            }
    //        } else
    //          evalResults.map {
    //            case Left(fs) => Left(b :: fs) // combine false evidence for false result
    //            case bs => bs // exclude false evidence for true result
    //          }
    //      }
    //      .value

    val (resultState, evalResult) =
      ca.foldLeft[(S, Eval[Either[List[B], NonEmptyList[B]]])]((init, Eval.now(Left(Nil)))) {
        case ((lastState, evalLastResult), a) =>
          val (b, nextState) = applyCondition(a, lastState)
          val isTrue = ExtractValue.asBoolean(b)
          val evalResult = {
            if (isTrue) {
              if (expr.shortCircuit) Eval.now(Right(NonEmptyList.of(b)))
              else
                evalLastResult.map {
                  case Left(_) => Right(NonEmptyList.of(b)) // this whole expression is now a true result
                  case Right(ts) => Right(b :: ts) // combine true evidence for true result
                }
            } else
              evalLastResult.map {
                case Left(fs) => Left(b :: fs) // combine false evidence for false result
                case bs => bs // exclude false evidence for true result
              }
          }
          (nextState, evalResult)
      }
    val falseOrTrueResults = evalResult.value
    val o = falseOrTrueResults.fold(expr.combineFalse, expr.combineTrue)
    (falseOrTrueResults, o, resultState)
  }
}

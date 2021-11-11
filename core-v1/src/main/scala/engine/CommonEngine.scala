package com.rallyhealth.vapors.v1

package engine

import algebra.Expr
import cats.data.NonEmptySeq
import cats.{Eval, Foldable, Functor}
import data.{Extract, ExtractValue}
import shapeless.Id

/**
  * Shared implementation for expression interpreters.
  *
  * @tparam S a higher-kinded type for caching the current state (must be able to extract a value, map over the state,
  *           and fold the current state with a new value into a new state).
  * @tparam OP the type of output parameter. This is unused, but must be defined to satisfy the compiler.
  */
abstract class CommonEngine[S[_] : Extract : Foldable : Functor, OP[_]] {
  import cats.implicits._

  private final val S = Extract[S]

  /**
    * Computes the given [[Expr.ForAll]] node by folding the initial state with the state produced
    * by computing each condition.
    *
    * @param expr the [[Expr.ForAll]] expression to interpret and compute
    * @param initialState the initial cached state of the interpreter with the foldable value inside
    * @param applyCondition use the cached result of each element to recursively interpret and compute
    *                       the condition result
    *
    * @tparam C the collection to perform the exists operation
    * @tparam A the element type
    * @tparam B the boolean-like condition result type
    * @return the cached result of either a non-empty Vector of the true results (if the expression
    *         short-circuits, then it will only return the first true result) OR all the true results
    */
  protected def visitForAllCommon[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
    expr: Expr.ForAll[C, A, B, OP],
    initialState: S[C[A]],
  )(
    applyCondition: S[A] => S[B],
  ): S[(Either[NonEmptySeq[B], Seq[B]], B)] = {
    // Create a foldable that folds over each value of the initial state
    val SC = Foldable[S].compose[C]
    // Replace the current value of the initial state with an empty Eval for computing the accumulated result
    // This is effectively creating an empty value with the same state as the initial state
    val acc: S[Eval[Either[NonEmptySeq[B], Seq[B]]]] = initialState.as(Eval.now(Right(Vector())))
    // Fold the initial state into the initially empty accumulator
    val finalState = SC.foldLeft(initialState, acc) {
      case (prevState, a) =>
        // Apply the given condition to get the next state and value
        val sb = applyCondition(prevState.as(a))
        // Extract the value
        val b = S.extract(sb)
        // Determine if the value is truthy
        val isTrue = ExtractValue.asBoolean(b)
        if (isTrue) {
          prevState.map { prevStep =>
            prevStep.map {
              case Right(ts) => Right(ts :+ b) // combine true evidence for true result
              case bs => bs // exclude true evidence for false result
            }
          }
        } else {
          // If we have a true condition AND we can short-circuit, then we are done
          // Just replace the latest computed state with this result and stop
          if (expr.shortCircuit) sb.as(Eval.now(Left(NonEmptySeq(b, Vector.empty))))
          else // we are not going to short-circuit, so keep collecting the values
            prevState.map { prevStep =>
              prevStep.map {
                case Right(_) => Left(NonEmptySeq(b, Vector.empty)) // this whole expression is now a false result
                case Left(fs) => Left(fs :+ b) // combine false evidence for false result
              }
            }
        }
    }
    // Run the evaluation and produce the final state
    finalState.map { computeResults =>
      val falseOrTrueResults = computeResults.value
      val finalOutput = falseOrTrueResults.fold(expr.combineFalse, expr.combineTrue)
      // Keep the intermediate results alongside the final output
      (falseOrTrueResults, finalOutput)
    }
  }

  /**
    * Computes the given [[Expr.Exists]] node by folding the initial state with the state produced
    * by computing each condition.
    *
    * @param expr the [[Expr.Exists]] expression to interpret and compute
    * @param initialState the initial cached state of the interpreter with the foldable value inside
    * @param applyCondition use the cached result of each element to recursively interpret and compute
    *                       the condition result
    *
    * @tparam C the collection to perform the exists operation
    * @tparam A the element type
    * @tparam B the boolean-like condition result type
    * @return the cached result of either all the true results OR a non-empty Vector of the false results
    *         (if the expression short-circuits, then it will only return the first false result)
    */
  protected def visitExistsCommon[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
    expr: Expr.Exists[C, A, B, OP],
    initialState: S[C[A]],
  )(
    applyCondition: S[A] => S[B],
  ): S[(Either[Seq[B], NonEmptySeq[B]], B)] = {
    // Create a foldable that folds over each value of the initial state
    val SC = Foldable[S].compose[C]
    // Replace the current value of the initial state with an empty Eval for computing the accumulated result
    // This is effectively creating an empty value with the same state as the initial state
    val acc: S[Eval[Either[Seq[B], NonEmptySeq[B]]]] = initialState.as(Eval.now(Left(Vector())))
    // Fold the initial state into the initially empty accumulator
    val finalState = SC.foldLeft(initialState, acc) {
      case (prevState, a) =>
        // Apply the given condition to get the next state and value
        val sb = applyCondition(prevState.as(a))
        // Extract the value
        val b = S.extract(sb)
        // Determine if the value is truthy
        val isTrue = ExtractValue.asBoolean(b)
        if (isTrue) {
          // If we have a true condition AND we can short-circuit, then we are done
          // Just replace the latest computed state with this result and stop
          if (expr.shortCircuit) sb.as(Eval.now(Right(NonEmptySeq(b, Vector.empty))))
          else // we are not going to short-circuit, so keep collecting the values
            prevState.map { prevStep =>
              prevStep.map {
                case Left(_) => Right(NonEmptySeq(b, Vector.empty)) // this whole expression is now a true result
                case Right(ts) => Right(ts :+ b) // combine true evidence for true result
              }
            }
        } else {
          prevState.map { prevStep =>
            prevStep.map {
              case Left(fs) => Left(fs :+ b) // combine false evidence for false result
              case bs => bs // exclude false evidence for true result
            }
          }
        }
    }
    // Run the evaluation and produce the final state
    finalState.map { computeResults =>
      val falseOrTrueResults = computeResults.value
      val finalOutput = falseOrTrueResults.fold(expr.combineFalse, expr.combineTrue)
      // Keep the intermediate results alongside the final output
      (falseOrTrueResults, finalOutput)
    }
  }
}

abstract class CommonUncachedEngine[OP[_]] extends CommonEngine[Id, OP] {

  override protected def visitForAllCommon[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
    expr: Expr.ForAll[C, A, B, OP],
    initialState: C[A],
  )(
    applyCondition: A => B,
  ): (Either[NonEmptySeq[B], Seq[B]], B) = super.visitForAllCommon(expr, initialState: Id[C[A]])(applyCondition)

  override protected def visitExistsCommon[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
    expr: Expr.Exists[C, A, B, OP],
    initialState: C[A],
  )(
    applyCondition: A => B,
  ): (Either[Seq[B], NonEmptySeq[B]], B) = super.visitExistsCommon(expr, initialState: Id[C[A]])(applyCondition)
}

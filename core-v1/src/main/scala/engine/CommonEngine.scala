package com.rallyhealth.vapors.v1

package engine

import algebra.Expr
import data.ExtractValue

import cats.data.NonEmptySeq
import cats.{Foldable, Monad}
import shapeless.Id

/**
  * Shared implementation for expression interpreters.
  *
  * @tparam S a higher-kinded type for caching the current state (must be able to extract a value, map over the state,
  *           and fold the current state with a new value into a new state).
  * @tparam OP the type of output parameter. This is unused, but must be defined to satisfy the compiler.
  */
abstract class CommonEngine[S[_] : Monad, OP[_]] {
  import cats.implicits._

  private final val S = Monad[S]

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
    // Replace the current value of the initial state with an empty Eval for computing the accumulated result
    // This is effectively creating an empty value with the same state as the initial state
    val acc: S[Either[NonEmptySeq[B], Seq[B]]] = initialState.as(Right(Vector()))

    // Flatten the updated state with the initial state to produce the final state
    val finalState = initialState.flatMap { values =>
      // Fold the values from the initial state into the initially empty accumulator
      if (expr.shortCircuit) {
        values.foldLeft(acc) {
          case (acc, a) =>
            // If we have a false condition result AND we can short-circuit, then we are done
            // Just replace the latest computed state with this result and stop
            acc.flatMap {
              case early @ Left(_) => S.pure(early)
              case _ => applyForAllCondition[C, A, B](applyCondition, acc, a)
            }
        }
      } else {
        // Same logic as short-circuit version but we don't stop on the first false condition result
        values.foldLeft(acc) {
          case (acc, a) => applyForAllCondition[C, A, B](applyCondition, acc, a)
        }
      }
    }
    // Combine the intermediate results into a single boolean output result
    finalState.map { falseOrTrueResults =>
      val finalOutput = falseOrTrueResults.fold(expr.combineFalse, expr.combineTrue)
      // Keep the intermediate results alongside the final output
      (falseOrTrueResults, finalOutput)
    }
  }

  /**
    * Helper method for determining the result of a forall fold operation step.
    *
    * Applies the condition function and accumulates either the next false or true result.
    *
    * @note If the result is false, then all previous true results will be dropped.
    *
    * @param applyCondition function to determine the condition result
    * @param acc the accumulated results so far
    * @param next the next value to pass to the condition function
    * @return a new state with the next result folded in
    */
  protected def applyForAllCondition[C[_] : Foldable, A, B : ExtractValue.AsBoolean](
    applyCondition: S[A] => S[B],
    acc: S[Either[NonEmptySeq[B], Seq[B]]],
    next: A,
  ): S[Either[NonEmptySeq[B], Seq[B]]] = {
    acc.flatMap { s =>
      // Apply the given condition to get the next state and value
      val sb = applyCondition(acc.as(next))
      sb.map { b =>
        // Determine if the value is truthy
        val isTrue = ExtractValue.asBoolean(b)
        if (isTrue) s match {
          case Right(ts) => Right(ts :+ b) // combine true evidence for true result
          case bs => bs // exclude true evidence for false result
        } else
          s match {
            case Right(_) => Left(NonEmptySeq(b, Vector.empty)) // this whole expression is now a false result
            case Left(fs) => Left(fs :+ b) // combine false evidence for false result
          }
      }
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
    // Replace the current value of the initial state with an empty Eval for computing the accumulated result
    // This is effectively creating an empty value with the same state as the initial state
    val acc: S[Either[Seq[B], NonEmptySeq[B]]] = initialState.as(Left(Vector()))
    // Flatten the updated state with the initial state to produce the final state
    val finalState = initialState.flatMap { values =>
      // Fold the values from the initial state into the initially empty accumulator
      if (expr.shortCircuit) {
        values.foldLeft(acc) {
          case (acc, a) =>
            // If we have a true condition result AND we can short-circuit, then we are done
            // Just replace the latest computed state with this result and stop
            acc.flatMap {
              case early @ Right(_) => S.pure(early)
              case _ => applyExistsCondition[C, A, B](applyCondition, acc, a)
            }
        }
      } else {
        // Same logic as short-circuit version but we don't stop on the first false condition result
        values.foldLeft(acc) {
          case (acc, a) => applyExistsCondition[C, A, B](applyCondition, acc, a)
        }
      }
    }
    // Combine the intermediate results into a single boolean output result
    finalState.map { falseOrTrueResults =>
      val finalOutput = falseOrTrueResults.fold(expr.combineFalse, expr.combineTrue)
      // Keep the intermediate results alongside the final output
      (falseOrTrueResults, finalOutput)
    }
  }

  /**
    * Helper method for determining the result of a exists fold operation step.
    *
    * Applies the condition function and accumulates either the next false or true result.
    *
    * @note If the result is true, then all previous false results will be dropped.
    *
    * @param applyCondition function to determine the condition result
    * @param acc the accumulated results so far
    * @param next the next value to pass to the condition function
    * @return a new state with the next result folded in
    */
  protected def applyExistsCondition[C[_] : Foldable, A, B : ExtractValue.AsBoolean](
    applyCondition: S[A] => S[B],
    acc: S[Either[Seq[B], NonEmptySeq[B]]],
    next: A,
  ): S[Either[Seq[B], NonEmptySeq[B]]] = {
    acc.flatMap { s =>
      // Apply the given condition to get the next state and value
      val sb = applyCondition(acc.as(next))
      sb.map { b =>
        // Determine if the value is truthy
        val isTrue = ExtractValue.asBoolean(b)
        if (isTrue) s match {
          case Left(_) => Right(NonEmptySeq(b, Vector.empty)) // this whole expression is now a true result
          case Right(ts) => Right(ts :+ b) // combine true evidence for true result
        } else
          s match {
            case Left(fs) => Left(fs :+ b) // combine false evidence for false result
            case bs => bs // exclude false evidence for true result
          }
      }
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

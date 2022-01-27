package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

/**
  * A type-level definition for how to get the head and tail for a non-empty [[ExprHList]]
  * (i.e. an [[ExprHCons]])
  *
  * @tparam L the [[Tuple]] type returned by the [[ExprHCons]]
  */
sealed abstract class IsExprHCons[L <: NonEmptyTuple] private {

  def head[I, OP[_]](xhl: ExprHList[I, L, OP]): Expr[I, Tuple.Head[L], OP]
  def tail[I, OP[_]](xhl: ExprHList[I, L, OP]): ExprHList[I, Tuple.Tail[L], OP]
}

object IsExprHCons {

  /**
    * Use the definition of [[IsHCons]] to determine the head and tail types of an [[ExprHCons]].
    *
    * @note while this uses runtime casting, it is safe by construction because of the way that
    *       [[Tuple]]s and [[ExprHList]]s are constructed in a parallel fashion.
    *
    * @param isHCons the proof that this [[ExprHList]] must be an [[ExprHCons]] because the embedded [[Tuple]]
    *                is a cons type (i.e. [[shapeless.::]])
    * @tparam L the output [[Tuple]] of the [[ExprHList]] that must be non-empty
    * @tparam H0 the head of the non-empty [[Tuple]]
    * @tparam T0 the tail of the non-empty [[Tuple]] (can be [[shapeless.HNil]])
    * @return an object that can be used to extract the head and tail of the [[ExprHCons]]
    */
  implicit def isXHCons[L <: NonEmptyTuple]: IsExprHCons[L] =
    new IsExprHCons[L] {

      override def head[I, OP[_]](xhl: ExprHList[I, L, OP]): Expr[I, Tuple.Head[L], OP] = xhl match {
        case ExprHCons(head, _) => head.asInstanceOf[Expr[I, Tuple.Head[L], OP]]
        case _ => ???
      }

      override def tail[I, OP[_]](xhl: ExprHList[I, L, OP]): ExprHList[I, Tuple.Tail[L], OP] = xhl match {
        case ExprHCons(_, tail) => tail.asInstanceOf[ExprHList[I, Tuple.Tail[L], OP]]
        case _ => ???
      }
    }
}

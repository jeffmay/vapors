package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import shapeless.ops.hlist.IsHCons
import shapeless.{unexpected, HList}

/**
  * A type-level definition for how to get the head and tail for a non-empty [[ExprHList]]
  * (i.e. an [[ExprHCons]])
  *
  * @tparam L the [[HList]] type returned by the [[ExprHCons]]
  */
sealed abstract class IsExprHCons[L <: HList] private {
  type H
  type T <: HList

  def head[I, OP[_]](xhl: ExprHList[I, L, OP]): Expr[I, H, OP]
  def tail[I, OP[_]](xhl: ExprHList[I, L, OP]): ExprHList[I, T, OP]
}

object IsExprHCons {

  type Aux[L <: HList, H0, T0 <: HList] = IsExprHCons[L] {
    type H = H0
    type T = T0
  }

  /**
    * Use the definition of [[IsHCons]] to determine the head and tail types of an [[ExprHCons]].
    *
    * @note while this uses runtime casting, it is safe by construction because of the way that
    *       [[HList]]s and [[ExprHList]]s are constructed in a parallel fashion.
    *
    * @param isHCons the proof that this [[ExprHList]] must be an [[ExprHCons]] because the embedded [[HList]]
    *                is a cons type (i.e. [[shapeless.::]])
    * @tparam L the output [[HList]] of the [[ExprHList]] that must be non-empty
    * @tparam H0 the head of the non-empty [[HList]]
    * @tparam T0 the tail of the non-empty [[HList]] (can be [[shapeless.HNil]])
    * @return an object that can be used to extract the head and tail of the [[ExprHCons]]
    */
  implicit def isXHCons[L <: HList, H0, T0 <: HList](implicit isHCons: IsHCons.Aux[L, H0, T0]): Aux[L, H0, T0] =
    new IsExprHCons[L] {
      override type H = H0
      override type T = T0

      override def head[I, OP[_]](xhl: ExprHList[I, L, OP]): Expr[I, H0, OP] = xhl match {
        case ExprHCons(head, _) => head.asInstanceOf[Expr[I, H0, OP]]
        case _ => unexpected
      }

      override def tail[I, OP[_]](xhl: ExprHList[I, L, OP]): ExprHList[I, T0, OP] = xhl match {
        case ExprHCons(_, tail) => tail.asInstanceOf[ExprHList[I, T0, OP]]
        case _ => unexpected
      }
    }
}

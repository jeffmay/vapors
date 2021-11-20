package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import shapeless.ops.hlist.IsHCons
import shapeless.{unexpected, HList}

trait IsExprHCons[L <: HList] {
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

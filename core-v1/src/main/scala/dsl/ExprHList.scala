package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import shapeless.{::, HList, HNil}

sealed trait ExprHList[-I, +L <: HList, OP[_]] {

  def ::[CI <: I, H](expr: Expr[CI, H, OP]): ExprHList[CI, H :: L, OP] = ExprHCons(expr, this)
}

object ExprHNil {
  @inline final def apply[OP[_]]: ExprHNil[OP] = nil.asInstanceOf[ExprHNil[OP]]
  private final object nil extends ExprHNil[Any]

  final def unapply(v: Any): Boolean = nil == v
}

sealed abstract class ExprHNil[OP[_]] extends ExprHList[Any, HNil, OP]

case class ExprHCons[-I, +H, +T <: HList, OP[_]](
  head: Expr[I, H, OP],
  tail: ExprHList[I, T, OP],
) extends ExprHList[I, H :: T, OP]

object ExprHList {

  implicit class Ops[I, L <: HList, OP[_]](private val xhl: ExprHList[I, L, OP]) {

    def head(implicit c: IsExprHCons[L]): Expr[I, c.H, OP] = c.head(xhl)

    def tail(implicit c: IsExprHCons[L]): ExprHList[I, c.T, OP] = c.tail(xhl)
  }
}

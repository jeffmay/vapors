//package com.rallyhealth.vapors.v1
//
//package dsl
//
//import algebra.{Expr, ExprHCons, ExprHLast}
//
//import shapeless._
//
//final class BuildExprHLast[I, W[_], O, OP[_]](private val last: Expr[I, W[O], OP]) extends AnyVal {
//
//  def ::[HI <: I, HO](head: Expr[HI, W[HO], OP]): ExprHCons[HI, W, HO, O :: HNil, W[O] :: HNil, OP] =
//    ExprHCons(head, ExprHLast(last))
//}

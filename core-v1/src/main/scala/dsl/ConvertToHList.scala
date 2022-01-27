package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import cats.arrow.Arrow
import cats.implicits._

trait ConvertToHList[L <: Tuple] {

  def convertToHListWith[G[-_, +_] : Arrow, I, OP[_]](
    xhl: ExprHList[I, L, OP],
    v: Expr.Visitor[G, OP],
  ): G[I, L]
}

object ConvertToHList {

  implicit def toHNil: ConvertToHList[EmptyTuple] =
    new ConvertToHList[EmptyTuple] {
      override def convertToHListWith[G[-_, +_] : Arrow, I, OP[_]](
        xhl: ExprHList[I, EmptyTuple, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, EmptyTuple] =
        Arrow[G].lift(_ => EmptyTuple)
    }

  implicit def toHCons[H, T <: Tuple](implicit toHL: ConvertToHList[T]): ConvertToHList[H *: T] =
    new ConvertToHList[H *: T] {
      override def convertToHListWith[G[-_, +_] : Arrow, I, OP[_]](
        xhl: ExprHList[I, H *: T, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, H *: T] = {
        val G = Arrow[G]
        val gh = xhl.head.visit(v)
        val gt = toHL.convertToHListWith(xhl.tail, v)
        (gh &&& gt) >>> G.lift { case (h, t) => h *: t }
      }
    }
}

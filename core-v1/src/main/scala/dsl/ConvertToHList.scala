package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import cats.arrow.Arrow
import cats.implicits._
import shapeless.{::, HList, HNil}

trait ConvertToHList[L <: HList] {

  def convertToHListWith[G[-_, +_] : Arrow, I, OP[_]](
    xhl: ExprHList[I, L, OP],
    v: Expr.Visitor[G, OP],
  ): G[I, L]
}

object ConvertToHList {

  implicit def toHNil: ConvertToHList[HNil] =
    new ConvertToHList[HNil] {
      override def convertToHListWith[G[-_, +_] : Arrow, I, OP[_]](
        xhl: ExprHList[I, HNil, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, HNil] =
        Arrow[G].lift(_ => HNil)
    }

  implicit def toHCons[H, T <: HList](implicit toHL: ConvertToHList[T]): ConvertToHList[H :: T] =
    new ConvertToHList[H :: T] {
      override def convertToHListWith[G[-_, +_] : Arrow, I, OP[_]](
        xhl: ExprHList[I, H :: T, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, H :: T] = {
        val G = Arrow[G]
        val gh = xhl.head.visit(v)
        val gt = toHL.convertToHListWith(xhl.tail, v)
        (gh &&& gt) >>> G.lift { case (h, t) => h :: t }
      }
    }
}

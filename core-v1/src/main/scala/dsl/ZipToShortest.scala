package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import cats.arrow.Arrow
import shapeless.HList

/**
  * Interprets an [[ExprHList]] using an [[Arrow]] to produce an F-wrapped [[HList]] of unwrapped results.
  *
  * For example (in pseudo-code types):
  *
  *     (I ~:> W[String]) :: (I ~:> W[Int]) => I ~:> W[String :: Int :: HNil]
  *
  * Or in a more concrete example:
  *
  *     (Any ~:> Seq[String]) :: (I ~:> Seq[Int]) => Any ~:> Seq[String :: Int :: HNil]
  */
trait ZipToShortest[W[_], WL <: HList, OP[_]] {
  type UL <: HList

  def zipToShortestWith[G[-_, +_] : Arrow, I](
    xhl: ExprHList[I, WL, OP],
    v: Expr.Visitor[G, OP],
  ): G[I, W[UL]]
}

/**
  * Implementations live in the subclasses of [[ExprHListDslImplicits]].
  */
object ZipToShortest {
  type Aux[W[_], WL <: HList, OP[_], UL0] = ZipToShortest[W, WL, OP] { type UL = UL0 }
}

package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import cats.arrow.Arrow

/**
  * Interprets an [[ExprHList]] using an [[Arrow]] to produce an F-wrapped [[Tuple]] of unwrapped results.
  *
  * For example (in pseudo-code types):
  *
  *     (I ~:> W[String]) :: (I ~:> W[Int]) => I ~:> W[String :: Int :: HNil]
  *
  * Or in a more concrete example:
  *
  *     (Any ~:> Seq[String]) :: (I ~:> Seq[Int]) => Any ~:> Seq[String :: Int :: HNil]
  */
trait ZipToShortest[W[_], WL <: Tuple, OP[_]] {
  type UL <: Tuple

  def zipToShortestWith[G[-_, +_] : Arrow, I](
    xhl: ExprHList[I, WL, OP],
    v: Expr.Visitor[G, OP],
  ): G[I, W[UL]]
}

/**
  * Implementations live in the subclasses of [[ExprHListDslImplicits]].
  */
object ZipToShortest {
  type Aux[W[_], WL <: Tuple, OP[_], UL0] = ZipToShortest[W, WL, OP] { type UL = UL0 }
}

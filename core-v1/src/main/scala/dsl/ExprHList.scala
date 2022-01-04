package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import shapeless.{::, HList, HNil}

/**
  * Analogous to an [[HList]], but for [[Expr]] nodes with a shared input type [[I]] and [[OP]] type.
  *
  * @see [[dsl.BuildExprDsl.ExprHListOpsBuilder]] for operations available on [[ExprHList]]
  *
  * @tparam I the shared input type for all expressions in the [[ExprHList]]
  * @tparam L the [[HList]] of output values (if this is a wrapped DSL, then each element will be wrapped)
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
sealed trait ExprHList[-I, +L <: HList, OP[_]] {

  /**
    * Prefix an expression node to this [[ExprHList]].
    *
    * @param headExpr the expression to prepend to this [[ExprHList]]
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam H the output type of the given head expression
    */
  def ::[CI <: I, H](headExpr: Expr[CI, H, OP]): ExprHList[CI, H :: L, OP] = ExprHCons(headExpr, this)
}

object ExprHList {

  implicit class Ops[I, L <: HList, OP[_]](private val xhl: ExprHList[I, L, OP]) {

    /**
      * Return the first element of this non-empty [[ExprHList]].
      */
    def head(implicit c: IsExprHCons[L]): Expr[I, c.H, OP] = c.head(xhl)

    /**
      * Return the tail elements of this non-empty [[ExprHList]].
      */
    def tail(implicit c: IsExprHCons[L]): ExprHList[I, c.T, OP] = c.tail(xhl)
  }
}

/**
  * Similar to [[HNil]], this represents an [[ExprHList]] with 0 elements.
  *
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
sealed abstract class ExprHNil[OP[_]] extends ExprHList[Any, HNil, OP]

object ExprHNil {
  @inline final def apply[OP[_]]: ExprHNil[OP] = nil.asInstanceOf[ExprHNil[OP]]
  private final object nil extends ExprHNil[Any]

  final def unapply(v: Any): Boolean = nil == v
}

/**
  * Similar to [[::]], this represents a conjunction of a definite [[head]] and a [[tail]] with 0 or more elements.
  *
  * @param head the first [[Expr]] element of this [[ExprHList]]
  * @param tail the remaining [[Expr]] elements wrapped in a [[ExprHList]]
  *             (either [[ExprHNil]] or [[ExprHCons]], depending on whether this is the last element or not)
  *
  * @tparam I the shared input type for all expressions in the [[ExprHList]]
  * @tparam H the return type of the head [[Expr]]
  * @tparam T the [[HList]] of all return types of the remaining elements in the [[ExprHList]]
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
case class ExprHCons[-I, +H, +T <: HList, OP[_]](
  head: Expr[I, H, OP],
  tail: ExprHList[I, T, OP],
) extends ExprHList[I, H :: T, OP]

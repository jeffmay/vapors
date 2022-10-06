package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

/**
  * Analogous to an [[Tuple]], but for [[Expr]] nodes with a shared input type [[I]] and [[OP]] type.
  *
  * @see [[dsl.BuildExprDsl.ExprHListOpsBuilder]] for operations available on [[ExprHList]]
  *
  * @tparam I the shared input type for all expressions in the [[ExprHList]]
  * @tparam L the [[Tuple]] of output values (if this is a wrapped DSL, then each element will be wrapped)
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
sealed trait ExprHList[-I, +L <: Tuple, OP[_]] {

  /**
    * Prefix an expression node to this [[ExprHList]].
    *
    * @param headExpr the expression to prepend to this [[ExprHList]]
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam H the output type of the given head expression
    */
  def *:[CI <: I, H](headExpr: Expr[CI, H, OP]): ExprHList[CI, H *: L, OP] = ExprHCons(headExpr, this)

  /**
    * Prefix an expression node to this [[ExprHList]].
    *
    * @param headExpr the expression to prepend to this [[ExprHList]]
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam H the output type of the given head expression
    */
  def ::[CI <: I, H](headExpr: Expr[CI, H, OP]): ExprHList[CI, H *: L, OP] = ExprHCons(headExpr, this)
}

object ExprHList {

  implicit def asExpr[I, L <: Tuple : ConvertToHList : OP, OP[_]](xhl: ExprHList[I, L, OP]): Expr.ToHList[I, L, OP] =
    Expr.ToHList(xhl)

  implicit class Ops[I, L <: Tuple, OP[_]](private val xhl: ExprHList[I, L, OP]) {

    /**
      * Return the first element of this non-empty [[ExprHList]].
      */
    def head[This >: L <: NonEmptyTuple]: Expr[I, Tuple.Head[This], OP] =
      xhl.asInstanceOf[ExprHCons[I, Tuple.Head[This], Tuple.Tail[This], OP]].head

    /**
      * Return the tail elements of this non-empty [[ExprHList]].
      */
    def tail[This >: L <: NonEmptyTuple]: ExprHList[I, Tuple.Tail[This], OP] =
      xhl.asInstanceOf[ExprHCons[I, Tuple.Head[This], Tuple.Tail[This], OP]].tail

    def toExpr(
      implicit
      c: ConvertToHList[L],
      opL: OP[L],
    ): Expr.ToHList[I, L, OP] = Expr.ToHList(xhl)
  }
}

/**
  * Similar to [[HNil]], this represents an [[ExprHList]] with 0 elements.
  *
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
sealed abstract class ExprHNil[OP[_]] extends ExprHList[Any, EmptyTuple, OP]

object ExprHNil {
  @inline final def apply[OP[_]]: ExprHNil[OP] = nil.asInstanceOf[ExprHNil[OP]]
  private object nil extends ExprHNil[[_] =>> Any]

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
  * @tparam T the [[Tuple]] of all return types of the remaining elements in the [[ExprHList]]
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
case class ExprHCons[-I, +H, +T <: Tuple, OP[_]](
  head: Expr[I, H, OP],
  tail: ExprHList[I, T, OP],
) extends ExprHList[I, H *: T, OP]

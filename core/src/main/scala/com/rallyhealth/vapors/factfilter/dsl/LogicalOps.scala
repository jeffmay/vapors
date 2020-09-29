package com.rallyhealth.vapors.factfilter.dsl

import com.rallyhealth.vapors.core.logic.{Intersect, Union}

final class LogicalOps[T, A](private val exp: Exp[T, A]) extends AnyVal {

  /**
    * Builds an AND expression using infix notation.
    */
  def and(o: Exp[T, A])(implicit A: Intersect[A]): Exp[T, A] = __.and(exp, o)

  /**
    * Same as [[and]] but with higher precedent.
    */
  def &&(o: Exp[T, A])(implicit A: Intersect[A]): Exp[T, A] = __.and(exp, o)

  /**
    * Builds an OR expression using infix notation.
    */
  def or(o: Exp[T, A])(implicit A: Union[A]): Exp[T, A] = __.or(exp, o)

  /**
    * Same as [[or]] but with higher precedent.
    */
  def ||(o: Exp[T, A])(implicit A: Union[A]): Exp[T, A] = __.or(exp, o)
}

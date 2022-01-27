package com.rallyhealth.vapors.v1.dsl

import shapeless3.deriving.Const

import scala.annotation.implicitAmbiguous

sealed abstract class NotEmpty[C[_], A] private {}

object NotEmpty extends NotEmpty[Const[Any], Any] {

  final val errorMessage =
    "There is no reason to call this method on an expression of ${C} { type A = Nothing }, as it will never be invoked."

  implicit def notEmpty[C[_], A]: NotEmpty[C, A] = NotEmpty.asInstanceOf[NotEmpty[C, A]]

  @implicitAmbiguous(errorMessage)
  implicit def empty1[C[_]]: NotEmpty[C, Nothing] = ???
  implicit def empty2[C[_]]: NotEmpty[C, Nothing] = ???
}

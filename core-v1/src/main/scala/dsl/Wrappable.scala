package com.rallyhealth.vapors.v1

package dsl

import shapeless.unexpected

import scala.annotation.implicitAmbiguous

trait Wrappable[A]

object Wrappable extends Wrappable[Any] {

  implicit def id[A]: Wrappable[A] = Wrappable.asInstanceOf[Wrappable[A]]

  @implicitAmbiguous(
    """
Can't use the .get operation to select a field of type Array[${A}].

This is because there is no Functor[Array] definition. You can use .getAs[Seq] to convert the value to a Seq before mapping over the results.""",
  )
  implicit def arrayIsNotWrappable1[A]: Wrappable[Array[A]] = unexpected
  implicit def arrayIsNotWrappable2[A]: Wrappable[Array[A]] = unexpected

  @implicitAmbiguous(
    """
Can't use the .get operation to select a field of type ${C}[${A}].

This is because there is no Functor[${C}] definition. You can use .getAs[Seq] to convert the value to a Seq before mapping over the results.""",
  )
  implicit def setIsNotWrappable1[C[a] <: Set[a], A]: Wrappable[C[A]] = unexpected
  implicit def setIsNotWrappable2[C[a] <: Set[a], A]: Wrappable[C[A]] = unexpected

}

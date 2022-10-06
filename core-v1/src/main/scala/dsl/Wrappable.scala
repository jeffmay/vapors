package com.rallyhealth.vapors.v1

package dsl

import scala.annotation.implicitAmbiguous

trait Wrappable[A]

object Wrappable extends Wrappable[Any] {

  def unexpected: Nothing = throw new Exception(s"This method was never intended to be invoked.")

  implicit def id[A]: Wrappable[A] = Wrappable.asInstanceOf[Wrappable[A]]

  @implicitAmbiguous(
    """
Can't use the .get operation to select a field of type Array[${A}].

This is because there is no Traverse[Array] definition. You can use .getAs[Seq] on the expression or .to(Seq) on the lens to convert the value to a Seq before mapping over the results.""",
  )
  implicit def arrayIsNotWrappable1[A]: Wrappable[Array[A]] = unexpected
  implicit def arrayIsNotWrappable2[A]: Wrappable[Array[A]] = unexpected

  @implicitAmbiguous(
    """
Can't use the .get operation to select a field of type Iterable[${A}].

This is because there is no Traverse[Iterable] definition. You can use .getAs[Seq] on the expression or .to(Seq) on the lens to convert the value to a Seq before mapping over the results.""",
  )
  implicit def iterableIsNotWrappable1[A]: Wrappable[Iterable[A]] = unexpected
  implicit def iterableIsNotWrappable2[A]: Wrappable[Iterable[A]] = unexpected

  @implicitAmbiguous(
    """
Can't use the .get operation to select a field of type IterableOnce[${A}].

This is because there is no Traverse[IterableOnce] definition. You can use .getAs[Seq] on the expression or .to(Seq) on the lens to convert the value to a Seq before mapping over the results.""",
  )
  implicit def iterableOnceIsNotWrappable1[A]: Wrappable[IterableOnce[A]] = unexpected
  implicit def iterableOnceIsNotWrappable2[A]: Wrappable[IterableOnce[A]] = unexpected

}

package com.rallyhealth.vapors.core.data

import cats.Functor

import scala.language.implicitConversions

/**
  * Magnet type for [[Fact]]s and lifted values.
  */
sealed trait Data[+A] {
  def isValueOnly: Boolean
}

final case class Value[+A](value: A) extends Data[A] {
  override def isValueOnly: Boolean = true
}

object Value {
  implicit object FunctorImpl extends Functor[Value] {
    override def map[A, B](fa: Value[A])(f: A => B): Value[B] = Value(f(fa.value))
  }
}

final case class Fact[+A](
  typeInfo: FactType[A],
  value: A,
) extends Data[A] {
  override def isValueOnly: Boolean = false
}

object Fact {

  final def lens[A]: NamedLens.Id[Fact[A]] = NamedLens.id[Fact[A]]

  final def value[A]: NamedLens[Fact[A], A] = {
    NamedLens[Fact[A], A](DataPath.empty.atField("value"), _.value)
  }
}

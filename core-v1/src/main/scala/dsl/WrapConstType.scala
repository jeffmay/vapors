package com.rallyhealth.vapors.v1

package dsl

import algebra.{Extract, WrapConst}

import scala.collection.Factory

/**
  * Defines the type-level behavior for wrapping constants.
  *
  * For example, iterable types will wrap all of their elements, and by default,
  * all other types will wrap the whole value using the implicit [[WrapConst]].
  *
  * @tparam W the wrapper type defined by [[WrapConst]]
  * @tparam A the const value type to wrap
  */
trait WrapConstType[W[_], A] {
  type Out

  def apply(wrapped: W[A]): Out
}

object WrapConstType extends LowPriorityWrapConstType {
  type Aux[W[_], A, O] = WrapConstType[W, A] { type Out = O }

  implicit def iterable[C[a] <: Iterable[a], W[_] : Extract : WrapConst, A](
    implicit
    factory: Factory[W[A], C[W[A]]],
  ): WrapConstType.Aux[W, C[A], C[W[A]]] =
    new WrapConstType[W, C[A]] {
      override type Out = C[W[A]]
      override final def apply(wrapped: W[C[A]]): C[W[A]] =
        Extract[W].extract(wrapped).map(WrapConst[W].wrapConst).to(factory)
    }

  implicit def nil[W[+_]]: WrapConstType.Aux[W, Nil.type, List[W[Nothing]]] =
    new WrapConstType[W, Nil.type] {
      override type Out = List[W[Nothing]]
      override def apply(wrapped: W[Nil.type]): List[W[Nothing]] = Nil
    }
}

trait LowPriorityWrapConstType {

  implicit def value[W[_], A]: WrapConstType.Aux[W, A, W[A]] =
    new WrapConstType[W, A] {
      override type Out = W[A]
      override final def apply(wrapped: W[A]): W[A] = wrapped
    }
}

package com.rallyhealth.vapors.v1

package algebra

import data.{Justified, Window}

import cats.Show
import cats.data.NonEmptyList

// TODO: Extract the "wrapped" and Functor capabilities from the comparison capability?
// TODO: Since this is no longer used on the Expr class, can this be made invariant again?
trait CompareWrapped[F[+_]] {

  def extract[A](wrapped: F[A]): A

  def map[A, B](fa: F[A])(ab: A => B): F[B]

  def wrapConst[A](value: A): F[A]

  def compare[V](
    value: F[V],
    window: F[Window[V]],
  ): F[Boolean]
}

object CompareWrapped {

  @inline final def apply[F[+_] : CompareWrapped]: CompareWrapped[F] = implicitly

  // Can't use cats.Id because this needs to be covariant
  type NoWrapper[+A] = A

  val value: CompareWrapped[NoWrapper] = {
    new CompareWrapped[NoWrapper] {

      override def map[A, B](fa: A)(ab: A => B): B = ab(fa)

      override def extract[V](wrapped: V): V = wrapped

      override def wrapConst[V](value: V): V = value

      override def compare[V](
        value: V,
        window: Window[V],
      ): Boolean = window.contains(value)
    }
  }

  val justified: CompareWrapped[Justified] = {
    new CompareWrapped[Justified] {

      override def extract[V](wrapped: Justified[V]): V = wrapped.value

      // TODO: Move this to a Functor definition?
      override def map[A, B](fa: Justified[A])(ab: A => B): Justified[B] = fa match {
        case Justified.ByConst(v) => Justified.byConst(ab(v))
        case Justified.ByConfig(v, k, d) => Justified.byConfig(ab(v), k, d)
        case _ =>
          val derived = ab(fa.value)
          Justified.byInference(s"[derive: $derived from ${fa.reason}]", derived, NonEmptyList.of(fa))
      }

      override def wrapConst[V](value: V): Justified[V] = Justified.byConst(value)

      override def compare[V](
        value: Justified[V],
        window: Justified[Window[V]],
      ): Justified[Boolean] = {
        val comparison = Window.showWindowWithTerm[V]("_")(Show.fromToString).show(window.value)
        val isWithinWindow = window.value.contains(value.value)
        Justified.byInference(comparison, isWithinWindow, NonEmptyList.of(value, window))
      }
    }
  }
}

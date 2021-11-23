package com.rallyhealth.vapors.v1

package algebra

import data.{Justified, Window}

import cats.Show
import cats.data.NonEmptyList
import shapeless.Id

trait CompareWrapped[F[_]] {

  def compare[V](
    value: F[V],
    window: F[Window[V]],
  ): F[Boolean]
}

object CompareWrapped {

  @inline final def apply[F[_] : CompareWrapped]: CompareWrapped[F] = implicitly

  val value: CompareWrapped[Id] = {
    new CompareWrapped[Id] {

      override def compare[V](
        value: V,
        window: Window[V],
      ): Boolean = window.contains(value)
    }
  }

  val justified: CompareWrapped[Justified] = {
    new CompareWrapped[Justified] {

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

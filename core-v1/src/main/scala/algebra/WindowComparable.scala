package com.rallyhealth.vapors.v1

package algebra

import data.{Justified, Window}
import debug.HasShow

import cats.data.NonEmptyList
import cats.{Id, Show}

trait WindowComparable[F[_], OP[_]] {

  // TODO: Should this take OP[V] and OP[Window[V]]? What about the F[_] wrapper?
  //       For now, I am only taking the OP[V] because if the goal is to serialize the value and the window,
  //       then that is the missing piece.
  def withinWindow[V](
    value: F[V],
    window: F[Window[V]],
  )(implicit
    opV: OP[V],
  ): F[Boolean]
}

object WindowComparable {

  private val anyJustified: WindowComparable[Justified, Any] = {
    new WindowComparable[Justified, Any] {

      override def withinWindow[V](
        value: Justified[V],
        window: Justified[Window[V]],
      )(implicit
        opV: Any,
      ): Justified[Boolean] = {
        implicit val showV: Show[V] = opV match {
          case has: HasShow[_] => has.show.asInstanceOf[Show[V]]
          case _ => Show.fromToString
        }
        val comparison = Window.showWindowWithTerm[V]("_").show(window.value)
        val isWithinWindow = Window.contains(window.value, value.value)
        // TODO: Combine evidence of window with evidence of value?
        Justified.byInference(comparison, isWithinWindow, NonEmptyList.of(value, window))
      }
    }
  }

  implicit def justified[OP[_]]: WindowComparable[Justified, OP] =
    anyJustified.asInstanceOf[WindowComparable[Justified, OP]]

  private val anyIdentity: WindowComparable[Lambda[a => a], Any] = new WindowComparable[Lambda[a => a], Any] {
    override def withinWindow[V](
      value: V,
      window: Window[V],
    )(implicit
      opV: Any,
    ): Boolean = Window.contains(window, value)
  }

  implicit def identity[OP[_]]: WindowComparable[Id, OP] = anyIdentity.asInstanceOf[WindowComparable[Id, OP]]
}

trait WrapConst[F[_]] {

  def wrapConst[A](value: A): F[A]
}

object WrapConst {

  @inline final def apply[F[_] : WrapConst]: WrapConst[F] = implicitly

  implicit val identity: WrapConst[Id] = new WrapConst[Lambda[a => a]] {
    override def wrapConst[A](value: A): A = value
  }

  implicit val justified: WrapConst[Justified] = new WrapConst[Justified] {
    override def wrapConst[A](value: A): Justified[A] = Justified.byConst(value)
  }
}

// TODO: Replace with cats Monoid or ZIO Prelude?
trait Extract[F[_]] {

  def extract[A](fa: F[A]): A
}

object Extract {

  @inline final def apply[F[_] : Extract]: Extract[F] = implicitly

  implicit val identity: Extract[Id] = new Extract[Lambda[a => a]] {
    override def extract[A](fa: A): A = fa
  }

  implicit val justified: Extract[Justified] = new Extract[Justified] {
    override def extract[A](fa: Justified[A]): A = fa.value
  }
}

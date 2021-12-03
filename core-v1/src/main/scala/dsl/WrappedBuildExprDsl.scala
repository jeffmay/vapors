package com.rallyhealth.vapors.v1

package dsl

import algebra.{Expr, Extract}
import data.Window

import cats.Functor

import scala.collection.Factory

trait WrappedBuildExprDsl extends BuildExprDsl with MidPriorityWrappedBuildExprDsl {
  self: DslTypes =>

  override def ident[I](implicit opI: OP[W[I]]): Expr.Identity[W[I], OP] = Expr.Identity()

  /**
    * Allows you to skip calling .const on a window to wrap it in a Expr.Const
    */
  implicit def wrapWindow[O](window: Window[O])(implicit opW: OP[W[Window[O]]]): Any ~:> W[Window[O]] =
    Expr.Const(wrapConst.wrapConst(window))

}

trait MidPriorityWrappedBuildExprDsl extends LowPriorityWrappedBuildExprDsl {
  self: BuildExprDsl with DslTypes =>

  implicit def hkIterable[I, C[a] <: IterableOnce[a], A](
    expr: I ~:> W[C[A]],
  )(implicit
    factory: Factory[W[A], C[W[A]]],
    opC: OP[C[W[A]]],
  ): SpecificHkExprBuilder[I, C, A] = hk {
    expr.andThen {
      Expr.CustomFunction[W[C[A]], C[W[A]], OP]("justifyElement", { outer =>
        val values = Extract[W].extract(outer)
        values.iterator.map(wrapElement(outer, _)).to(factory)
      })
    }
  }
}

trait LowPriorityWrappedBuildExprDsl {
  self: BuildExprDsl with DslTypes =>

  protected def wrapElement[C[_], A](
    outer: W[C[A]],
    element: A,
  ): W[A]

  import cats.implicits._

  implicit def hkFunctor[I, C[_] : Functor, A](
    expr: I ~:> W[C[A]],
  )(implicit
    opC: OP[C[W[A]]],
  ): SpecificHkExprBuilder[I, C, A] = hk {
    expr.andThen {
      Expr.CustomFunction[W[C[A]], C[W[A]], OP]("justifyElement", { outer =>
        val values = Extract[W].extract(outer)
        values.map(wrapElement(outer, _))
      })
    }
  }
}

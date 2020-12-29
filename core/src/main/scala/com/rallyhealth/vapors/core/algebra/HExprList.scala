package com.rallyhealth.vapors.core.algebra

import cats.syntax.all._
import cats.{Functor, Semigroupal}
import shapeless.{::, HList, HNil}

// TODO: How to convert this back into an HList of Expr nodes?
sealed trait NonEmptyExprHList[F[_], V, T <: HList, P] {

  def ::[H](other: Expr[F, V, H, P]): NonEmptyExprHList[F, V, H :: T, P] =
    new ExprHCons[F, V, H, T, P](other, this)

  def visit[G[_] : Functor : Semigroupal](v: Expr.Visitor[F, V, P, G]): G[T]
}

final case class ExprHCons[F[_], V, H, T <: HList, P](
  head: Expr[F, V, H, P],
  tail: NonEmptyExprHList[F, V, T, P],
) extends NonEmptyExprHList[F, V, H :: T, P] {
  override def visit[G[_] : Functor : Semigroupal](v: Expr.Visitor[F, V, P, G]): G[H :: T] = {
    (head.visit(v), tail.visit(v)).mapN(_ :: _)
  }
}

final case class ExprLast[F[_], V, H, P](last: Expr[F, V, H, P]) extends NonEmptyExprHList[F, V, H :: HNil, P] {
  override def visit[G[_] : Functor : Semigroupal](v: Expr.Visitor[F, V, P, G]): G[H :: HNil] = {
    last.visit(v).map(_ :: HNil)
  }
}

package com.rallyhealth.vapors.v1

package algebra

import cats.arrow.Arrow
import cats.implicits._
import cats.{Functor, Semigroupal}
import shapeless.ops.hlist.Comapped
import shapeless.{::, HList, HNil}

// TODO: Remove extra head param?
sealed abstract class NonEmptyExprHList[-I, F[+_], +WL <: HList, +UL <: HList, OP[_]](
  implicit
  comapped: Comapped.Aux[WL, F, UL],
) {

  def ::[PI <: I, PH](expr: Expr[PI, F[PH], OP]): ExprHCons[PI, F, PH, WL, UL, OP] =
    ExprHCons(expr, this)

  def concatToHListWith[G[-_, +_]](v: ConcatToHList.Visitor[F, G, OP]): G[I, WL]

  def zipToHListWith[G[-_, +_]](v: ZipToHList.Visitor[F, G, OP]): G[I, F[UL]]

}

final case class ExprHCons[-I, F[+_], +H, +WT <: HList, +UT <: HList, OP[_]](
  head: Expr[I, F[H], OP],
  tail: NonEmptyExprHList[I, F, WT, UT, OP],
)(implicit
  comapped: Comapped.Aux[F[H] :: WT, F, H :: UT],
) extends NonEmptyExprHList[I, F, F[H] :: WT, H :: UT, OP] {

  override def concatToHListWith[G[-_, +_]](v: ConcatToHList.Visitor[F, G, OP]): G[I, F[H] :: WT] =
    v.visitHCons(this)

  override def zipToHListWith[G[-_, +_]](v: ZipToHList.Visitor[F, G, OP]): G[I, F[H :: UT]] =
    v.visitHCons(this)
}

final case class ExprHLast[-I, F[+_], +H, OP[_]](last: Expr[I, F[H], OP])
  extends NonEmptyExprHList[I, F, F[H] :: HNil, H :: HNil, OP] {

  override def concatToHListWith[G[-_, +_]](v: ConcatToHList.Visitor[F, G, OP]): G[I, F[H] :: HNil] =
    v.visitHLast(this)

  override def zipToHListWith[G[-_, +_]](v: ZipToHList.Visitor[F, G, OP]): G[I, F[H :: HNil]] =
    v.visitHLast(this)
}

object ConcatToHList {

  def proxy[F[+_], G[-_, +_] : Arrow, OP[_]](exprVisitor: Expr.Visitor[G, OP]): Visitor[F, G, OP] =
    new ProxyVisitor(exprVisitor)

  trait Visitor[F[+_], G[-_, +_], OP[_]] {

    def visitHCons[I, H, WT <: HList, UT <: HList](xhl: ExprHCons[I, F, H, WT, UT, OP]): G[I, F[H] :: WT]

    def visitHLast[I, H](xhl: ExprHLast[I, F, H, OP]): G[I, F[H] :: HNil]
  }

  class ProxyVisitor[F[+_], G[-_, +_] : Arrow, OP[_]](exprVisitor: Expr.Visitor[G, OP]) extends Visitor[F, G, OP] {

    private final val G: Arrow[G] = implicitly

    override def visitHCons[I, H, WT <: HList, UT <: HList](xhl: ExprHCons[I, F, H, WT, UT, OP]): G[I, F[H] :: WT] = {
      val headResult = xhl.head.visit(exprVisitor)
      val tailResult = xhl.tail.concatToHListWith(this)
      (headResult &&& tailResult) >>> G.lift { case (fh, ft) => fh :: ft }
    }

    override def visitHLast[I, H](xhl: ExprHLast[I, F, H, OP]): G[I, F[H] :: HNil] = {
      val lastResult = xhl.last.visit(exprVisitor)
      lastResult >>> G.lift { _ :: HNil }
    }
  }
}

object ZipToHList {

  def proxy[F[+_] : Functor : Semigroupal, G[-_, +_] : Arrow, OP[_]](
    exprVisitor: Expr.Visitor[G, OP],
  ): Visitor[F, G, OP] = new ProxyVisitor(exprVisitor)

  trait Visitor[F[+_], G[-_, +_], OP[_]] {

    def visitHLast[I, H](xhl: ExprHLast[I, F, H, OP]): G[I, F[H :: HNil]]

    def visitHCons[I, H, WT <: HList, UT <: HList](xhl: ExprHCons[I, F, H, WT, UT, OP]): G[I, F[H :: UT]]
  }

  class ProxyVisitor[F[+_] : Functor : Semigroupal, G[-_, +_] : Arrow, OP[_]](exprVisitor: Expr.Visitor[G, OP])
    extends Visitor[F, G, OP] {

    private val G: Arrow[G] = implicitly

    def visitHLast[I, H](xhl: ExprHLast[I, F, H, OP]): G[I, F[H :: HNil]] = {
      xhl.last.visit(exprVisitor) >>> G.lift(_.map(_ :: HNil))
    }

    def visitHCons[I, H, WT <: HList, UT <: HList](xhl: ExprHCons[I, F, H, WT, UT, OP]): G[I, F[H :: UT]] = {
      val headResult = xhl.head.visit(exprVisitor)
      val tailResult = xhl.tail.zipToHListWith(this)
      val result = (headResult &&& tailResult) >>> G.lift {
        case (fh, ft) =>
          (fh, ft).mapN { (h, t) =>
            h :: t
          }
      }
      result
    }
  }
}

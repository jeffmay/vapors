package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr

import cats.arrow.Arrow
import cats.data.Ior
import cats.implicits._
import cats.{Align, Functor, FunctorFilter, Semigroupal}
import shapeless.{::, HList, HNil, Id}

/**
  * Interprets an [[ExprHList]] using an [[Arrow]] to produce an F-wrapped [[HList]] of unwrapped results.
  *
  * For example (in pseudo-code types):
  *
  *     (I ~:> W[String]) :: (I ~:> W[Int]) => I ~:> W[String :: Int :: HNil]
  *
  * Or in a more concrete example:
  *
  *     (Any ~:> Seq[String]) :: (I ~:> Seq[Int]) => Any ~:> Seq[String :: Int :: HNil]
  */
trait ZipToShortest[W[_], WL <: HList, OP[_]] {
  type UL <: HList

  def zipToShortestWith[G[-_, +_] : Arrow, I](
    xhl: ExprHList[I, WL, OP],
    v: Expr.Visitor[G, OP],
  ): G[I, W[UL]]
}

object ZipToShortest extends MidPriorityZipToShortest {
  type Aux[W[_], WL <: HList, OP[_], UL0] = ZipToShortest[W, WL, OP] { type UL = UL0 }

  implicit def hlastIdMapN[L <: HList, H, OP[_]](
    implicit
    isCons: IsExprHCons.Aux[L, H, HNil],
  ): ZipToShortest.Aux[Id, L, OP, H :: HNil] =
    new ZipToShortest[Id, L, OP] {
      override type UL = H :: HNil
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, L, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, H :: HNil] = {
        val G = Arrow[G]
        xhl.head.visit(v).andThen(G.lift(_ :: HNil))
      }
    }

  implicit def hconsIdMapN[L <: HList, H, T <: HList, OP[_]](
    implicit
    isCons: IsExprHCons.Aux[L, H, T],
    mt: ZipToShortest.Aux[Id, T, OP, T],
  ): ZipToShortest.Aux[Id, L, OP, H :: T] =
    new ZipToShortest[Id, L, OP] {
      override type UL = H :: T
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, L, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, H :: T] = {
        val G = Arrow[G]
        val gh = xhl.head.visit(v)
        val gt = mt.zipToShortestWith(xhl.tail, v)
        (gh &&& gt) >>> G.lift {
          case (h, t) => h :: t
        }
      }
    }
}

sealed trait MidPriorityZipToShortest extends LowPriorityZipToShortest {

  implicit def hlastAlignMapN[C[_] : Functor, W[_] : Functor, H, OP[_]](
    implicit
    isCons: IsExprHCons.Aux[C[W[H]] :: HNil, C[W[H]], HNil],
  ): ZipToShortest.Aux[Lambda[a => C[W[a]]], C[W[H]] :: HNil, OP, H :: HNil] =
    new ZipToShortest[Lambda[a => C[W[a]]], C[W[H]] :: HNil, OP] {
      override type UL = H :: HNil
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, C[W[H]] :: HNil, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, C[W[H :: HNil]]] = {
        val G = Arrow[G]
        val gcwh = xhl.head.visit(v)
        gcwh >>> G.lift { cwh =>
          cwh.map { wh =>
            wh.map { h =>
              h :: HNil
            }
          }
        }
      }
    }

  implicit def hconsAlignMapN[C[_] : Align : FunctorFilter, W[_] : Functor : Semigroupal, H, WT <: HList, OP[_]](
    implicit
    isCons: IsExprHCons.Aux[C[W[H]] :: WT, C[W[H]], WT],
    mt: ZipToShortest[Lambda[a => C[W[a]]], WT, OP],
  ): ZipToShortest.Aux[Lambda[a => C[W[a]]], C[W[H]] :: WT, OP, H :: mt.UL] =
    new ZipToShortest[Lambda[a => C[W[a]]], C[W[H]] :: WT, OP] {
      override type UL = H :: mt.UL
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, C[W[H]] :: WT, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, C[W[H :: mt.UL]]] = {
        val C = Align[C]
        val G = Arrow[G]
        val gcwh = xhl.head.visit(v)
        val gcwt: G[I, C[W[mt.UL]]] = mt.zipToShortestWith(xhl.tail, v)
        (gcwh &&& gcwt) >>> G.lift {
          case (cwh, cwt) =>
            C.alignWith(cwh, cwt) {
                case Ior.Both(wh, wt) => Some((wh, wt).mapN(_ :: _))
                case _ => None
              }
              .collect {
                case Some(hl) => hl
              }
        }
      }
    }
}

sealed trait LowPriorityZipToShortest {

  implicit def hlastMapN[W[_] : Functor, H, OP[_]](
    implicit
    isCons: IsExprHCons.Aux[W[H] :: HNil, W[H], HNil],
  ): ZipToShortest.Aux[W, W[H] :: HNil, OP, H :: HNil] =
    new ZipToShortest[W, W[H] :: HNil, OP] {
      override type UL = H :: HNil
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, W[H] :: HNil, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, W[H :: HNil]] = {
        val G = Arrow[G]
        val head = isCons.head(xhl)
        val gfh = head.visit(v)
        gfh >>> G.lift(_.map(_ :: HNil))
      }
    }

  implicit def hconsMapN[W[_] : Functor : Semigroupal, H, WT <: HList, OP[_]](
    implicit
    isCons: IsExprHCons.Aux[W[H] :: WT, W[H], WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, W[H] :: WT, OP, H :: mt.UL] =
    new ZipToShortest[W, W[H] :: WT, OP] {
      override type UL = H :: mt.UL
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, W[H] :: WT, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, W[H :: mt.UL]] = {
        val G = Arrow[G]
        val head = isCons.head(xhl)
        val tail = isCons.tail(xhl)
        val gfh = head.visit(v)
        val gft = mt.zipToShortestWith(tail, v)
        (gfh &&& gft) >>> G.lift {
          case (fh, ft) =>
            (fh, ft).mapN { (h, t) =>
              h :: t
            }
        }
      }
    }
}

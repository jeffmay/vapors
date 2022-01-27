package com.rallyhealth.vapors.v1.dsl

import cats.{Align, Functor, FunctorFilter, Semigroupal, Traverse}
import cats.arrow.Arrow
import cats.data.Ior
import cats.implicits._
import com.rallyhealth.vapors.v1.algebra.Expr
import com.rallyhealth.vapors.v1.lens.DataPath

import scala.collection.Factory

final class DslImplicitDefinitions[W[+_] : Functor : Semigroupal, OP[_]](
  implicit
  wrapElementW: WrapSelected[W, OP],
  wrapConstW: WrapConst[W, OP],
) {

  def constIterable[C[a] <: Iterable[a], A, O](
    sot: SelectOutputType.Aux[W, C[A], A, O],
  )(implicit
    factory: Factory[O, C[O]],
    opCA: OP[C[A]],
  ): ConstOutputType.Aux[W, C[A], C[O]] =
    new ConstOutputType[W, C[A]] {
      override type Out = C[O]
      override def wrapConst(value: C[A]): C[O] = {
        val wrappedConst = wrapConstW.wrapConst(value)
        value.zipWithIndex
          .map {
            case (a, idx) =>
              sot.wrapSelected(wrappedConst, DataPath.empty.atIndex(idx), a)
          }
          .to(factory)
      }
    }

  def constTraverse[C[_] : Traverse, O](
    sot: SelectOutputType[W, C[O], O],
  )(implicit
    opCO: OP[C[O]],
  ): ConstOutputType.Aux[W, C[O], C[sot.Out]] =
    new ConstOutputType[W, C[O]] {
      override type Out = C[sot.Out]
      override def wrapConst(value: C[O]): C[sot.Out] = {
        val wrappedConst = wrapConstW.wrapConst(value)
        value.mapWithIndex { (a, idx) =>
          sot.wrapSelected(wrappedConst, DataPath.empty.atIndex(idx), a)
        }
      }
    }

  def selectOption[I : OP, A : OP, O](
    sot: SelectOutputType.Aux[W, I, A, O],
  ): SelectOutputType.Aux[W, I, Option[A], Option[O]] = selectTraverse(sot)

  def selectIterable[C[a] <: Iterable[a], I : OP, A : OP, O](
    sot: SelectOutputType.Aux[W, I, A, O],
  )(implicit
    factory: Factory[O, C[O]],
  ): SelectOutputType.Aux[W, I, C[A], C[O]] =
    new SelectOutputType[W, I, C[A]] {
      override type Out = C[O]
      override def wrapSelected(
        wrapped: W[I],
        path: DataPath,
        value: C[A],
      ): C[O] = {
        value.zipWithIndex
          .map {
            case (a, i) =>
              sot.wrapSelected(wrapped, path.atIndex(i), a)
          }
          .to(factory)
      }
    }

  def selectTraverse[C[_] : Traverse, I : OP, A : OP, O](
    sot: SelectOutputType.Aux[W, I, A, O],
  ): SelectOutputType.Aux[W, I, C[A], C[O]] =
    new SelectOutputType[W, I, C[A]] {
      override type Out = C[O]
      override def wrapSelected(
        wrapped: W[I],
        path: DataPath,
        value: C[A],
      ): C[O] = {
        value.mapWithIndex { (a, i) =>
          sot.wrapSelected(wrapped, path.atIndex(i), a)
        }
      }
    }

  def constId[O : OP]: ConstOutputType.Aux[W, O, W[O]] =
    new ConstOutputType[W, O] {
      override type Out = W[O]
      override def wrapConst(value: O): W[O] = wrapConstW.wrapConst(value)
    }

  def selectId[I : OP, O : OP]: SelectOutputType.Aux[W, I, O, W[O]] =
    new SelectOutputType[W, I, O] {
      override type Out = W[O]
      override def wrapSelected(
        wrapped: W[I],
        path: DataPath,
        value: O,
      ): W[O] = wrapElementW.wrapSelected(wrapped, path, value)
    }

  def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons[C[W[H]] *: EmptyTuple],
  ): ZipToShortest.Aux[[a] =>> C[W[a]], C[W[H]] *: EmptyTuple, OP, H *: EmptyTuple] =
    new ZipToShortest[[a] =>> C[W[a]], C[W[H]] *: EmptyTuple, OP] {
      override type UL = H *: EmptyTuple
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, C[W[H]] *: EmptyTuple, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, C[W[H *: EmptyTuple]]] = {
        val G = Arrow[G]
        val gcwh = xhl.head.visit(v)
        gcwh >>> G.lift { cwh =>
          cwh.map { wh =>
            wh.map { h =>
              h *: EmptyTuple
            }
          }
        }
      }
    }

  def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: Tuple](
    mt: ZipToShortest[[a] =>> C[W[a]], WT, OP],
  )(implicit
    isCons: IsExprHCons[C[W[H]] *: WT],
  ): ZipToShortest.Aux[[a] =>> C[W[a]], C[W[H]] *: WT, OP, H *: mt.UL] =
    new ZipToShortest[[a] =>> C[W[a]], C[W[H]] *: WT, OP] {
      override type UL = H *: mt.UL
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, C[W[H]] *: WT, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, C[W[H *: mt.UL]]] = {
        val C = Align[C]
        val G = Arrow[G]
        val gcwh = xhl.head.visit(v)
        val gcwt: G[I, C[W[mt.UL]]] = mt.zipToShortestWith(xhl.tail, v)
        (gcwh &&& gcwt) >>> G.lift {
          case (cwh, cwt) =>
            C.alignWith(cwh, cwt) {
                case Ior.Both(wh, wt) => Some((wh, wt).mapN(_ *: _))
                case _ => None
              }
              .collect {
                case Some(hl) => hl
              }
        }
      }
    }

  def hlastMapN[H](
    implicit
    isCons: IsExprHCons[W[H] *: EmptyTuple],
  ): ZipToShortest.Aux[W, W[H] *: EmptyTuple, OP, H *: EmptyTuple] =
    new ZipToShortest[W, W[H] *: EmptyTuple, OP] {
      override type UL = H *: EmptyTuple
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, W[H] *: EmptyTuple, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, W[H *: EmptyTuple]] = {
        val G = Arrow[G]
        val head = isCons.head(xhl)
        val gfh = head.visit(v)
        gfh >>> G.lift(_.map(_ *: EmptyTuple))
      }
    }

  def hconsMapN[H, WT <: Tuple](
    mt: ZipToShortest[W, WT, OP],
  )(implicit
    isCons: IsExprHCons[W[H] *: WT],
  ): ZipToShortest.Aux[W, W[H] *: WT, OP, H *: mt.UL] =
    new ZipToShortest[W, W[H] *: WT, OP] {
      override type UL = H *: mt.UL
      override def zipToShortestWith[G[-_, +_] : Arrow, I](
        xhl: ExprHList[I, W[H] *: WT, OP],
        v: Expr.Visitor[G, OP],
      ): G[I, W[H *: mt.UL]] = {
        val G = Arrow[G]
        val head = isCons.head(xhl)
        val tail = isCons.tail(xhl)
        val gfh = head.visit(v)
        val gft = mt.zipToShortestWith(tail, v)
        (gfh &&& gft) >>> G.lift {
          case (fh, ft) =>
            (fh, ft).mapN { (h, t) =>
              h *: t
            }
        }
      }
    }
}

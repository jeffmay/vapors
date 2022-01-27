package com.rallyhealth.vapors.v1.dsl

import cats.{Align, Functor, FunctorFilter}

trait DefaultUnwrappedExprHListImplicits
  extends UnwrappedExprHListDslImplicits
  with DefaultUnwrappedLowPriorityExprHListDslImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons[C[H] *: EmptyTuple, C[H], EmptyTuple],
  ): ZipToShortest.Aux[C, C[H] *: EmptyTuple, OP, H *: EmptyTuple] = {
    // Help IntelliJ prove that the W[_] wrapper can be removed in phases
    val zts: ZipToShortest.Aux[[a] =>> C[a], C[W[H]] *: EmptyTuple, OP, H *: EmptyTuple] =
      defn.hlastAlignMapN[C, H](Functor[C], isCons)
    zts
  }

  override implicit final def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: Tuple](
    implicit
    isCons: IsExprHCons[C[H] *: WT],
    mt: ZipToShortest[C, WT, OP],
  ): ZipToShortest.Aux[C, C[H] *: WT, OP, H *: mt.UL] = {
    // IntelliJ highlights that as an error, but it compiles fine
    defn.hconsAlignMapN(mt)
  }
}

trait DefaultUnwrappedLowPriorityExprHListDslImplicits
  extends UnwrappedLowPriorityExprHListDslImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit def hlastMapN[H](
    implicit
    isCons: IsExprHCons[H *: EmptyTuple],
  ): ZipToShortest.Aux[W, H *: EmptyTuple, OP, H *: EmptyTuple] =
    defn.hlastMapN

  override implicit def hconsMapN[H, WT <: Tuple](
    implicit
    isCons: IsExprHCons[H *: WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, H *: WT, OP, H *: mt.UL] = defn.hconsMapN(mt)
}

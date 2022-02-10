package com.rallyhealth.vapors.v1.dsl

import cats.{Align, Functor, FunctorFilter}
import shapeless.{::, HList, HNil}

trait DefaultUnwrappedExprHListImplicits
  extends UnwrappedExprHListDslImplicits
  with DefaultUnwrappedLowPriorityExprHListDslImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit final def hlastAlignIterableOnceMapN[H](
    implicit
    isCons: IsExprHCons.Aux[IterableOnce[H] :: HNil, IterableOnce[H], HNil],
  ): ZipToShortest.Aux[Seq, IterableOnce[H] :: HNil, OP, H :: HNil] =
    defn.hlastAlignIterableOnceMapN

  override implicit final def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons.Aux[C[H] :: HNil, C[H], HNil],
  ): ZipToShortest.Aux[C, C[H] :: HNil, OP, H :: HNil] = {
    // Help IntelliJ prove that the W[_] wrapper can be removed in phases
    val zts: ZipToShortest.Aux[Lambda[a => C[a]], C[W[H]] :: HNil, OP, H :: HNil] =
      defn.hlastAlignMapN[C, H](Functor[C], isCons)
    zts
  }

  override implicit final def hconsAlignIterableOnceMapN[H, WT <: HList](
    implicit
    mt: ZipToShortest[Seq, WT, OP],
    isCons: IsExprHCons.Aux[IterableOnce[H] :: WT, IterableOnce[H], WT],
  ): ZipToShortest.Aux[Seq, IterableOnce[H] :: WT, OP, H :: mt.UL] =
    defn.hconsAlignIterableOnceMapN(mt)

  override implicit final def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[C[H] :: WT, C[H], WT],
    mt: ZipToShortest[C, WT, OP],
  ): ZipToShortest.Aux[C, C[H] :: WT, OP, H :: mt.UL] = {
    // IntelliJ highlights that as an error, but it compiles fine
    defn.hconsAlignMapN(mt)
  }
}

trait DefaultUnwrappedLowPriorityExprHListDslImplicits
  extends UnwrappedLowPriorityExprHListDslImplicits
  with DefaultUnwrappedDslImplicitDefinitions {

  override implicit def hlastMapN[H](
    implicit
    isCons: IsExprHCons.Aux[H :: HNil, H, HNil],
  ): ZipToShortest.Aux[W, H :: HNil, OP, H :: HNil] =
    defn.hlastMapN

  override implicit def hconsMapN[H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[H :: WT, H, WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, H :: WT, OP, H :: mt.UL] = defn.hconsMapN(mt)
}

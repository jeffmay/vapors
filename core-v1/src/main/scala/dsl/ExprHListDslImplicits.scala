package com.rallyhealth.vapors.v1

package dsl

import cats.{Align, Functor, FunctorFilter}
import shapeless.{::, HList, HNil}

import scala.collection.Factory

/**
  * A marker trait for determining which set of implicits to inherit.
  *
  * Unfortunately, IntelliJ IDEA and the compiler disagree about the return type when
  * using type lambdas versus type aliases. This approach allows use to choose the correct
  * type of implicits to satisfy both the compiler and IntelliJ IDEA based on whether you
  * are using a DSL with a wrapper class or one without.
  *
  * If your DSL uses a wrapper type, then extend the [[WrappedExprHListDslImplicits]],
  * otherwise extend the [[UnwrappedExprHListDslImplicits]].
  */
sealed trait ExprHListDslImplicits

/**
  * Extend this trait to provide the template for the standard implicit definitions (with appropriate priority).
  *
  * Every subclass must extend a separate subclass of [[WrappedLowPriorityExprHListDslImplicits]] and can just implement each
  * abstract implicit def by invoking the `defn` method of the same name.
  */
trait WrappedExprHListDslImplicits extends ExprHListDslImplicits {
  self: DslTypes with WrappedLowPriorityExprHListDslImplicits =>

  implicit def hlastAlignIterableOnceMapN[H](
    implicit
    isCons: IsExprHCons.Aux[IterableOnce[W[H]] :: HNil, IterableOnce[W[H]], HNil],
  ): ZipToShortest.Aux[Lambda[a => Seq[W[a]]], IterableOnce[W[H]] :: HNil, OP, H :: HNil]

  implicit def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons.Aux[C[W[H]] :: HNil, C[W[H]], HNil],
  ): ZipToShortest.Aux[Lambda[a => C[W[a]]], C[W[H]] :: HNil, OP, H :: HNil]

  implicit def hconsAlignIterableOnceMapN[H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[IterableOnce[W[H]] :: WT, IterableOnce[W[H]], WT],
    mt: ZipToShortest[Lambda[a => Seq[W[a]]], WT, OP],
  ): ZipToShortest.Aux[Lambda[a => Seq[W[a]]], IterableOnce[W[H]] :: WT, OP, H :: mt.UL]

  implicit def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[C[W[H]] :: WT, C[W[H]], WT],
    mt: ZipToShortest[Lambda[a => C[W[a]]], WT, OP],
  ): ZipToShortest.Aux[Lambda[a => C[W[a]]], C[W[H]] :: WT, OP, H :: mt.UL]

}

trait WrappedLowPriorityExprHListDslImplicits {
  self: DslTypes =>

  implicit def hlastMapN[H](
    implicit
    isCons: IsExprHCons.Aux[W[H] :: HNil, W[H], HNil],
  ): ZipToShortest.Aux[W, W[H] :: HNil, OP, H :: HNil]

  implicit def hconsMapN[H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[W[H] :: WT, W[H], WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, W[H] :: WT, OP, H :: mt.UL]
}

trait UnwrappedExprHListDslImplicits extends ExprHListDslImplicits {
  self: DslTypes with UnwrappedLowPriorityExprHListDslImplicits =>

  implicit def hlastAlignIterableOnceMapN[H](
    implicit
    isCons: IsExprHCons.Aux[IterableOnce[H] :: HNil, IterableOnce[H], HNil],
  ): ZipToShortest.Aux[Seq, IterableOnce[H] :: HNil, OP, H :: HNil]

  implicit def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons.Aux[C[H] :: HNil, C[H], HNil],
  ): ZipToShortest.Aux[C, C[H] :: HNil, OP, H :: HNil]

  implicit def hconsAlignIterableOnceMapN[H, WT <: HList](
    implicit
    mt: ZipToShortest[Seq, WT, OP],
    isCons: IsExprHCons.Aux[IterableOnce[H] :: WT, IterableOnce[H], WT],
  ): ZipToShortest.Aux[Seq, IterableOnce[H] :: WT, OP, H :: mt.UL]

  implicit def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[C[H] :: WT, C[H], WT],
    mt: ZipToShortest[C, WT, OP],
  ): ZipToShortest.Aux[C, C[H] :: WT, OP, H :: mt.UL]
}

trait UnwrappedLowPriorityExprHListDslImplicits {
  self: DslTypes =>

  implicit def hlastMapN[H](
    implicit
    isCons: IsExprHCons.Aux[H :: HNil, H, HNil],
  ): ZipToShortest.Aux[W, H :: HNil, OP, H :: HNil]

  implicit def hconsMapN[H, WT <: HList](
    implicit
    isCons: IsExprHCons.Aux[H :: WT, H, WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, H :: WT, OP, H :: mt.UL]
}

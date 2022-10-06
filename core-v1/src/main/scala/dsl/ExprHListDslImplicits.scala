package com.rallyhealth.vapors.v1

package dsl

import cats.{Align, Functor, FunctorFilter}

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
  self: DslTypes & WrappedLowPriorityExprHListDslImplicits =>

  implicit def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons[C[W[H]] *: EmptyTuple],
  ): ZipToShortest.Aux[[a] =>> C[W[a]], C[W[H]] *: EmptyTuple, OP, H *: EmptyTuple]

  implicit def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: Tuple](
    implicit
    isCons: IsExprHCons[C[W[H]] *: WT],
    mt: ZipToShortest[[a] =>> C[W[a]], WT, OP],
  ): ZipToShortest.Aux[[a] =>> C[W[a]], C[W[H]] *: WT, OP, H *: mt.UL]

}

trait WrappedLowPriorityExprHListDslImplicits {
  self: DslTypes =>

  implicit def hlastMapN[H](
    implicit
    isCons: IsExprHCons[W[H] *: EmptyTuple],
  ): ZipToShortest.Aux[W, W[H] *: EmptyTuple, OP, H *: EmptyTuple]

  implicit def hconsMapN[H, WT <: Tuple](
    implicit
    isCons: IsExprHCons[W[H] *: WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, W[H] *: WT, OP, H *: mt.UL]
}

trait UnwrappedExprHListDslImplicits extends ExprHListDslImplicits {
  self: DslTypes & UnwrappedLowPriorityExprHListDslImplicits =>

  implicit def hlastAlignMapN[C[_] : Functor, H](
    implicit
    isCons: IsExprHCons[C[H] *: EmptyTuple],
  ): ZipToShortest.Aux[C, C[H] *: EmptyTuple, OP, H *: EmptyTuple]

  implicit def hconsAlignMapN[C[_] : Align : FunctorFilter, H, WT <: Tuple](
    implicit
    isCons: IsExprHCons[C[H] *: WT],
    mt: ZipToShortest[C, WT, OP],
  ): ZipToShortest.Aux[C, C[H] *: WT, OP, H *: mt.UL]
}

trait UnwrappedLowPriorityExprHListDslImplicits {
  self: DslTypes =>

  implicit def hlastMapN[H](
    implicit
    isCons: IsExprHCons[H *: EmptyTuple],
  ): ZipToShortest.Aux[W, H *: EmptyTuple, OP, H *: EmptyTuple]

  implicit def hconsMapN[H, WT <: Tuple](
    implicit
    isCons: IsExprHCons[H *: WT],
    mt: ZipToShortest[W, WT, OP],
  ): ZipToShortest.Aux[W, H *: WT, OP, H *: mt.UL]
}

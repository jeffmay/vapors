package com.rallyhealth.vapors.v1

package dsl

import algebra.Expr
import shapeless.HList

/**
  * All type aliases defined by the [[FullDsl]] subclasses.
  */
trait DslTypes extends Any {

  /**
    * The type of output parameter to collect at each node of the expression.
    *
    * These parameters are made available to the DSL subclasses when performing various computations and
    * can provide extra context about the captured type of value.
    *
    * A couple use cases:
    * - An implicit that captures the source code information of the node, such as filename and line number
    * - An implicit encoder that defines how to serialize the value as Json
    * - An implicit that captures information required to define [[algebra.WrapConst]]
    *
    * It is generally a good idea to define a trait that has typeclass instances as members of the trait,
    * rather than putting the exact typeclass here.
    *
    * <h3>Example</h3>
    *
    * If you want to define a custom parameter that includes [[cats.Show]] information as well as your own
    * custom Codec[_] trait, you could create a custom trait that combines the two:
    *
    * {{{
    *   trait MyCustomParam[A] extends HasShow[A] {
    *     def codec: Codec[A]
    *   }
    *
    *   object MyCustomParam {
    *     private final case class Impl[A](codec: Codec[A], show: Show[A]) extends MyCustomParam[A]
    *     implicit def enc[A](implicit codec: Codec[A], show: HasShow[A]): MyCustomParam[A] = Impl(codec, show.show)
    *   }
    *
    *   object MyCustomDsl extends FullDsl with SimpleRunDsl with UnwrappedBuildExprDsl {
    *     override type OP[a] = MyCustomParam[a]
    *   }
    * }}}
    *
    * This pattern can be extended again in the same way that [[debug.HasShow]] is extended above.
    *
    * <hr/>
    *
    * @note the invariant type parameter allows any type constructor to be used here.
    * @note `type OP[_] = X[_]` is not the same as `type OP[a] = X[a]`.
    */
  type OP[O]

  /**
    * The type of wrapper (for example [[data.Justified]] or [[shapeless.Id]]).
    *
    * @note the covariant type parameter restricts what type constructors can be used here, however,
    *       since the output type of the [[Expr]] is covariant, this wrapper must be covariant as well.
    * @note `type W[_] = X[_]` is not the same as `type W[a] = X[a]`.
    */
  type W[+V]

  /**
    * Alias for [[Expr]] where the [[OP]] type is fixed.
    *
    * This makes it easy to use this type alias with infix notation (i.e. `~:>[I, O]` can be written `I ~:> O`)
    */
  final type ~:>[-I, +O] = Expr[I, O, OP]

  /**
    * @see [[ExprFunction]]
    */
  final type =~:>[I, +O] = ExprFunction[I, O]

  /**
    * A function that takes the identity expression `I ~:> I` as input and produces an expression of type `I ~:> O`
    * as output.
    *
    * @tparam I the input (and output) type of the expression provided as input to the function
    * @tparam O the output type of the expression returned by the function
    */
  final type ExprFunction[I, +O] = Expr[I, I, OP] => Expr[I, O, OP]

  /**
    * Alias for any expression with the `OP` type fixed by this DSL.
    */
  final type AnyExpr = Expr.AnyWith[OP]

  /**
    * Alias for a chained [[Expr.AndThen]] expression with `I ~:> M` followed by `M ~:> O`
    */
  final type AndThen[-I, M, +O] = Expr.AndThen[I, M, M, O, OP]

  /**
    * Alias for an [[Expr.WithinWindow]] where the output of a value expression `I ~:> W[V]` is checked for whether
    * it falls between the window defined by an internal expression of `I ~:> W[ Window[V] ]`.
    *
    * @note the symbol `>=<` is meant to look like the operators that are enabled by this `>`, `===`, `<`, `>=`, etc.
    *       as well as the boundaries of a closed range.
    */
  final type >=<[-I, +V] = Expr.WithinWindow[I, V, W, OP]

  /**
    * An [[ExprHList]] with a fixed [[OP]] type.
    */
  final type XHL[-I, +L <: HList] = ExprHList[I, L, OP]

  /**
    * An [[ExprHNil]] with a fixed [[OP]] type.
    */
  final type XHNil = ExprHNil[OP]

  /**
    * An alias for constructing [[XHNil]] that is not suitable for pattern matching
    * (you must use the [[ExprHNil.unapply]] method to match on this type properly)
    */
  @inline final def XHNil: XHNil = ExprHNil[OP]
}

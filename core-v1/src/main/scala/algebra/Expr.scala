package com.rallyhealth.vapors.v1

package algebra

import data._
import debug.{DebugArgs, Debugging, NoDebugging}
import dsl.{ConvertToHList, ExprHList, ExprHNil, Sortable, ZipToShortest}
import lens.{CollectInto, VariantLens}
import logic.{Conjunction, Disjunction, Negation}
import math._

import cats.data.{NonEmptySeq, NonEmptyVector}
import cats.{FlatMap, Foldable, Functor, Traverse}
import shapeless.{::, HList, HNil}

import scala.annotation.nowarn

/**
  * The root trait of all expression nodes.
  *
  * An expression can be interpreted to produce a function, a JSON object, or really, anything you want!
  * You can define an [[algebra.Expr.Visitor]] over both the input and output parameter and custom output
  * param ([[OP]]) and recurse by invoking the [[visit]] method on each node.
  *
  * The expression node is contravariant on the input and covariant on the output so that it feels a lot more
  * like native [[Function]]s and expressions that operate on [[Seq]] can be chained to an expression that
  * returns a [[List]]. Likewise, expressions that ignore the input can be defined without any fancy implicit
  * footwork to embed a [[algebra.Expr.Const]] expression.
  *
  * The use of variance requires imposes some limitations and requires some fancy type-level tricks to work
  * around this.
  *
  * The expression defines a set of useful operations (modeled after the Scala collections library) that
  * contain enough information to be serializable.
  * @see [[dsl.FullDsl]] for more information about how this works.
  *
  * @tparam I the input value type
  * @tparam O the output value type
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL).
  *            See [[dsl.DslTypes.OP]] for more details.
  */
sealed abstract class Expr[-I, +O : OP, OP[_]](val name: String) extends Product with Equals {

  /**
    * Prefix an expression node to create an [[ExprHList]] of size 2.
    *
    * @see [[dsl.BuildExprDsl.ExprHListOpsBuilder]] for operations available on [[ExprHList]]
    *
    * @param headExpr the expression to prepend to the constructed [[ExprHList]]
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam H the output type of the given head expression
    */
  def ::[CI <: I, H](headExpr: Expr[CI, H, OP]): ExprHList[CI, H :: O :: HNil, OP] = headExpr :: this :: ExprHNil[OP]

  /**
    * Holds any [[Debugging]] hook to be run while running this expression.
    *
    * You should favor using the `.debug` extension method provided by the [[DebugArgs.Attacher]] using an
    * implicit conversion of this [[Expr]].
    *
    * @note Only one hook can be attached to an expression. The last one attached wins.
    *       TODO: Remove this limitation by chaining debug functions together.
    *
    * @note As a top-level [[Expr]] method, this is not very useful, as you cannot invoke it, however, you can
    *       print or inspect it for information at runtime; and placing it here prevents me from forgetting to
    *       add this to subclasses.
    *
    * @see [[Debugging]] for more details on how type checking is handled.
    */
  private[v1] def debugging: Debugging[Nothing, Nothing]

  /**
    * Allows you to attach a debug hook that handles any input. Subclasses should override this to be more
    * specific about the acceptable input.
    *
    * @note Only one hook can be attached to an expression. The last one attached wins.
    *       TODO: Remove this limitation by chaining debug functions together.
    *
    * @note At the top-level [[Expr]], this is not very useful, as any debug hook attached will have no type
    *       information about the state of the expression, however, you can print or inspect the values at
    *       runtime in the hook; and placing it here prevents me from forgetting to add this to subclasses.
    *       The types are refined when attaching the debug hooks using the [[DebugArgs]] compiler trick.
    *
    * @see [[com.rallyhealth.vapors.v1.dsl.DebugExprDsl.debugExpr]] for how to get better type information
    *      at compile-time.
    */
  private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Expr[I, O, OP]

  /**
    * Helpful when inside of a long chain of expressions when you need to refer to this expression, but don't
    * want to have to convert the surrounding scope into a block and create a val statement and then return
    * the expression again in the final line of the block.
    *
    * Useful in combination with `.debug` when you want to inspect the [[Expr]] as well as the result state.
    */
  final def withSelf[A](action: this.type => A): A = action(this)

  /**
    * Calls the given visitor with the specific method for this subclass.
    *
    * @see [[Expr.Visitor]]
    */
  def visit[G[-_, +_]](v: Expr.Visitor[G, OP]): G[I, O]

  /**
    * Passes the output of this expression to the input of the expression given to create
    * an expression that returns the output of that expression.
    *
    * @see [[Expr.AndThen]] for more details
    */
  def andThen[OI >: O, OO : OP](that: Expr[OI, OO, OP]): Expr.AndThen[I, O, OI, OO, OP] =
    Expr.AndThen(this, that)

  /**
    * Add the given expression to this expression using the implicit definition for addition.
    *
    * @see [[Add]] for how to define new combinations of types that can be added.
    * @see [[CombineHolder]] for details on how type-inference works.
    *
    * @param that the other expression to add
    * @param add the type-level definition of how to add `this` output type to `that` output type
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam LI the left-input to the add operation must be a supertype of the output of `this` expression
    * @tparam RI the right-input to the add operation must be a supertype of the output of `that` given expression
    * @tparam RO the output of `that` given operation (must be a subtype of `RI`)
    *
    * @return a [[CombineHolder]] to allow for type-level calculation of the return type
    */
  def +[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    add: Add[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "add", add.add(_, _): @nowarn)
  }

  /**
    * Subtract the given expression from this expression using the implicit definition for subtraction.
    *
    * @see [[Subtract]] for how to define new combinations of types that can be subtracted.
    *
    * @param that the other expression to subtract from the output of this expression
    * @param sub the type-level definition of how to subtract `that` output type from `this` output type
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam LI the left-input to the subtract operation must be a supertype of the output of `this` expression
    * @tparam RI the right-input to the subtract operation must be a supertype of the output of `that` given expression
    * @tparam RO the output of `that` given operation (must be a subtype of `RI`)
    *
    * @return a [[CombineHolder]] to allow for type-level calculation of the return type
    */
  def -[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    sub: Subtract[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, sub.Out, OP] =
    new CombineHolder(this, that, "minus", sub.subtract(_, _): @nowarn)

  /**
    * Multiply the given expression to this expression using the implicit definition for multiplication.
    *
    * @see [[Multiply]] for how to define new combinations of types that can be multiplied.
    * @see [[CombineHolder]] for details on how type-inference works.
    *
    * @param that the other expression to multiply
    * @param mult the type-level definition of how to multiply `this` output type to `that` output type
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam LI the left-input to the multiply operation must be a supertype of the output of `this` expression
    * @tparam RI the right-input to the multiply operation must be a supertype of the output of `that` given expression
    * @tparam RO the output of `that` given operation (must be a subtype of `RI`)
    *
    * @return a [[CombineHolder]] to allow for type-level calculation of the return type
    */
  def *[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    mult: Multiply[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, mult.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "multiply", mult.multiply(_, _): @nowarn)
  }

  /**
    * Divide the given expression into this expression using the implicit definition for division.
    *
    * @see [[Divide]] for how to define new combinations of types that can be divided into each other.
    * @see [[CombineHolder]] for details on how type-inference works.
    *
    * @param that the other expression to multiply
    * @param div the type-level definition of how to divide `this` output type by `that` output type
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam LI the left-input to the divide operation must be a supertype of the output of `this` expression
    * @tparam RI the right-input to the divide operation must be a supertype of the output of `that` given expression
    * @tparam RO the output of `that` given operation (must be a subtype of `RI`)
    *
    * @return a [[CombineHolder]] to allow for type-level calculation of the return type
    */
  def /[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    div: Divide[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, div.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "divide", div.divide(_, _): @nowarn)
  }

  /**
    * Raise this expression to the power of the result of the given expression using the implicit
    * definition for exponentiation [[Power]].
    *
    * @see [[Divide]] for how to define new combinations of types that can be divided into each other.
    * @see [[CombineHolder]] for details on how type-inference works.
    *
    * @param that the other expression to raise this output to the power of
    * @param pow the type-level definition of how to raise `this` output type to the exponent of `that` output type
    *
    * @tparam CI a more specific input type to obey the laws of contravariance
    * @tparam LI the left-input to the power operation must be a supertype of the output of `this` expression
    * @tparam RI the right-input to the power operation must be a supertype of the output of `that` given expression
    * @tparam RO the output of `that` given operation (must be a subtype of `RI`)
    *
    * @return a [[CombineHolder]] to allow for type-level calculation of the return type
    */
  def ^[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    pow: Power[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, pow.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "pow", pow.power(_, _): @nowarn)
  }

  // TODO: Match on self and convert to string recursively as lazy val
  override def toString: String = s"$name[$hashCode]"
}

object Expr {

  final type AnyWith[OP[_]] = Expr[Nothing, Any, OP]

  /**
    * This is the magic that allows you to traverse the entire expression recursively. You need a
    * visitor type that is contravariant for the final input type and covariant for the final output type.
    *
    * The visitor is also parameterized on the "output parameter" `OP[_]`, which is supplied at every node
    * within the given expression tree. This can be useful information, such as how to serialize the output
    * of every subexpression, or information about the source code position provided by the compiler, etc.
    *
    * Every [[Expr]] subclass will call the associated `visitExpr` method and pass the required constraints.
    *
    * @tparam ~:> an infix type alias for the higher-kinded result type of this visitor
    *             (the symbol was chosen to indicate that more work must be performed by the visitor to produce
    *             a value of the output type and it will most often be interpreted as a `Function`)
    * @tparam OP `Output Parameter` (captured at the definition site for every output type in the expression tree).
    *            See [[dsl.DslTypes.OP]] for more details.
    */
  trait Visitor[~:>[-_, +_], OP[_]] {

    def visitAnd[I, B, W[+_]](
      expr: And[I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opB: OP[W[B]],
    ): I ~:> W[B]

    def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: AndThen[II, IO, OI, OO, OP],
    )(implicit
      evIOisOI: IO <:< OI,
    ): II ~:> OO

    def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): I ~:> O

    def visitConst[O : OP](expr: Const[O, OP]): Any ~:> O

    def visitConvert[I, O : OP](expr: Convert[I, O, OP]): I ~:> O

    def visitCustomFunction[I, O : OP](expr: CustomFunction[I, O, OP]): I ~:> O

    def visitDefine[I, C[_] : Foldable, T](
      expr: Define[I, C, T, OP],
    )(implicit
      opF: OP[Seq[TypedFact[T]]],
    ): I ~:> Seq[TypedFact[T]]

    def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](expr: Exists[C, A, B, OP]): C[A] ~:> B

    def visitFilter[C[_], A, B : ExtractValue.AsBoolean, D[_]](
      expr: Filter[C, A, B, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): C[A] ~:> D[A]

    def visitFlatten[C[_] : FlatMap, A](expr: Flatten[C, A, OP])(implicit opCA: OP[C[A]]): C[C[A]] ~:> C[A]

    def visitFoldLeft[I, C[_] : Foldable, A, O : OP](expr: FoldLeft[I, C, A, O, OP]): I ~:> O

    def visitForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](expr: ForAll[C, A, B, OP]): C[A] ~:> B

    def visitIdentity[I : OP](expr: Identity[I, OP]): I ~:> I

    def visitIsEqual[I, V, W[+_]](
      expr: IsEqual[I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): I ~:> W[Boolean]

    def visitGetOrElse[I, O : OP](expr: GetOrElse[I, O, OP]): I ~:> O

    def visitMapEvery[C[_] : Functor, A, B](expr: MapEvery[C, A, B, OP])(implicit opO: OP[C[B]]): C[A] ~:> C[B]

    def visitNot[I, B, W[+_]](
      expr: Not[I, B, W, OP],
    )(implicit
      logic: Negation[W, B, OP],
      opB: OP[W[B]],
    ): I ~:> W[B]

    def visitOr[I, B, W[+_]](
      expr: Or[I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): I ~:> W[B]

    def visitSelect[I, A, B, O : OP](expr: Select[I, A, B, O, OP]): I ~:> O

    def visitSequence[C[+_] : Traverse, I, O](expr: Sequence[C, I, O, OP])(implicit opCO: OP[C[O]]): I ~:> C[O]

    def visitSizeIs[I, N : ExtractValue[*, Int], B : ExtractValue.AsBoolean : OP](
      expr: SizeIs[I, N, B, OP],
    )(implicit
      compare: SizeComparable[I, N, B],
    ): I ~:> B

    def visitSlice[C[_] : Traverse, A, D[_]](
      expr: Slice[C, A, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): C[A] ~:> D[A]

    def visitSorted[C[_], A](
      expr: Sorted[C, A, OP],
    )(implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): C[A] ~:> C[A]

    def visitToHList[I, L <: HList : OP](expr: ToHList[I, L, OP])(implicit toHL: ConvertToHList[L]): I ~:> L

    def visitUsingDefinitions[I, O : OP](expr: UsingDefinitions[I, O, OP]): I ~:> O

    def visitValuesOfType[T, O](expr: ValuesOfType[T, O, OP])(implicit opTs: OP[Seq[O]]): Any ~:> Seq[O]

    def visitWhen[I, B : ExtractValue.AsBoolean, O : OP](expr: When[I, B, O, OP]): I ~:> O

    def visitWithinWindow[I, V, W[+_]](
      expr: WithinWindow[I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opV: OP[W[V]],
      opW: OP[W[Window[V]]],
      opB: OP[W[Boolean]],
    ): I ~:> W[Boolean]

    def visitZipToShortestHList[I, W[+_], WL <: HList, UL <: HList](
      expr: Expr.ZipToShortestHList[I, W, WL, UL, OP],
    )(implicit
      zip: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[W[UL]],
    ): I ~:> W[UL]
  }

  /**
    * A simple function definition for a function defined over 2 higher-kinded arity-2 type constructors,
    * [[G]] and [[H]], alongside a custom output parameter [[OP]].
    *
    * This is primarily used by [[ProxyVisitor]], but it could be seen as a general definition for the most
    * generic transformer for any [[Expr]] operation.
    */
  trait ProxyFunction[G[-_, +_], H[-_, +_], OP[_]] {
    def apply[I, O : OP](g: => G[I, O]): H[I, O]
  }

  /**
    * Proxy all method invocations to an [[underlying]] [[Visitor]] into a given [[ProxyFunction]] for pre / post
    * processing.
    *
    * @note You can override any specific method to do something other than proxy the visitor invocation.
    *       Alternatively, you can ignore the given visitor invocation and do something entirely different,
    *       however, it is unlikely you can do much with the input or output type, since the only constraint
    *       is on the output type, and it will not likely provide much power.
    *
    * @param proxy the [[ProxyFunction]] that will receive all method invocations (by default)
    * @param underlying the [[Visitor]] to invoke (by default) when proxying the method
    * @tparam G the initial interpreter result (parameterized over the input and output)
    * @tparam H the transformed interpreter result (parameterized over the same input and output)
    * @tparam OP `Output Parameter` (captured at the definition site for every output type in the expression tree).
    *            See [[dsl.DslTypes.OP]] for more details.
    */
  class ProxyVisitor[G[-_, +_], H[-_, +_], OP[_]](
    proxy: ProxyFunction[G, H, OP],
    underlying: Visitor[G, OP],
  ) extends Visitor[H, OP] {

    override def visitAnd[I, B, W[+_]](
      expr: And[I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opB: OP[W[B]],
    ): H[I, W[B]] = proxy(underlying.visitAnd(expr))

    override def visitAndThen[II, IO : OP, OI, OO : OP](
      expr: AndThen[II, IO, OI, OO, OP],
    )(implicit
      evIOisOI: IO <:< OI,
    ): H[II, OO] = proxy(underlying.visitAndThen(expr))

    override def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): H[I, O] = proxy(underlying.visitCombine(expr))

    override def visitConst[O : OP](expr: Const[O, OP]): H[Any, O] = proxy(underlying.visitConst(expr))

    override def visitConvert[I, O : OP](expr: Convert[I, O, OP]): H[I, O] = proxy(underlying.visitConvert(expr))

    override def visitCustomFunction[I, O : OP](expr: CustomFunction[I, O, OP]): H[I, O] =
      proxy(underlying.visitCustomFunction(expr))

    override def visitDefine[I, C[_] : Foldable, T](
      expr: Define[I, C, T, OP],
    )(implicit
      opF: OP[Seq[TypedFact[T]]],
    ): H[I, Seq[TypedFact[T]]] = proxy(underlying.visitDefine(expr))

    override def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: Exists[C, A, B, OP],
    ): H[C[A], B] =
      proxy(underlying.visitExists(expr))

    override def visitFilter[C[_], A, B : ExtractValue.AsBoolean, D[_]](
      expr: Filter[C, A, B, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): H[C[A], D[A]] = proxy(underlying.visitFilter(expr))

    override def visitFlatten[C[_] : FlatMap, A](expr: Flatten[C, A, OP])(implicit opCA: OP[C[A]]): H[C[C[A]], C[A]] =
      proxy(underlying.visitFlatten(expr))

    override def visitFoldLeft[I, C[_] : Foldable, A, B : OP](expr: FoldLeft[I, C, A, B, OP]): H[I, B] =
      proxy(underlying.visitFoldLeft(expr))

    override def visitForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](
      expr: ForAll[C, A, B, OP],
    ): H[C[A], B] =
      proxy(underlying.visitForAll(expr))

    override def visitIdentity[I : OP](expr: Identity[I, OP]): H[I, I] = proxy(underlying.visitIdentity(expr))

    override def visitIsEqual[I, V, W[+_]](
      expr: IsEqual[I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): H[I, W[Boolean]] = proxy(underlying.visitIsEqual(expr))

    override def visitGetOrElse[I, O : OP](expr: GetOrElse[I, O, OP]): H[I, O] = proxy(underlying.visitGetOrElse(expr))

    override def visitMapEvery[C[_] : Functor, A, B](
      expr: MapEvery[C, A, B, OP],
    )(implicit
      opO: OP[C[B]],
    ): H[C[A], C[B]] = proxy(underlying.visitMapEvery(expr))

    override def visitNot[I, B, F[+_]](
      expr: Not[I, B, F, OP],
    )(implicit
      logic: Negation[F, B, OP],
      opB: OP[F[B]],
    ): H[I, F[B]] = proxy(underlying.visitNot(expr))

    override def visitOr[I, B, W[+_]](
      expr: Or[I, B, W, OP],
    )(implicit
      logic: Disjunction[W, B, OP],
      opO: OP[W[B]],
    ): H[I, W[B]] = proxy(underlying.visitOr(expr))

    override def visitSelect[I, A, B, O : OP](expr: Select[I, A, B, O, OP]): H[I, O] =
      proxy(underlying.visitSelect(expr))

    override def visitSequence[C[+_] : Traverse, I, O](
      expr: Sequence[C, I, O, OP],
    )(implicit
      opCO: OP[C[O]],
    ): H[I, C[O]] = proxy(underlying.visitSequence(expr))

    override def visitSorted[C[_], A](
      expr: Sorted[C, A, OP],
    )(implicit
      sortable: Sortable[C, A],
      opAs: OP[C[A]],
    ): H[C[A], C[A]] = proxy(underlying.visitSorted(expr))

    override def visitSizeIs[I, N : ExtractValue[*, Int], B : ExtractValue.AsBoolean : OP](
      expr: SizeIs[I, N, B, OP],
    )(implicit
      compare: SizeComparable[I, N, B],
    ): H[I, B] = proxy(underlying.visitSizeIs(expr))

    override def visitUsingDefinitions[I, O : OP](expr: UsingDefinitions[I, O, OP]): H[I, O] =
      proxy(underlying.visitUsingDefinitions(expr))

    override def visitSlice[C[_] : Traverse, A, D[_]](
      expr: Slice[C, A, D, OP],
    )(implicit
      filter: CollectInto.Filter[C, A, D],
      opO: OP[D[A]],
    ): H[C[A], D[A]] =
      proxy(underlying.visitSlice(expr))

    override def visitValuesOfType[T, O](expr: ValuesOfType[T, O, OP])(implicit opTs: OP[Seq[O]]): H[Any, Seq[O]] =
      proxy(underlying.visitValuesOfType(expr))

    override def visitWhen[I, B : ExtractValue.AsBoolean, O : OP](expr: When[I, B, O, OP]): H[I, O] =
      proxy(underlying.visitWhen(expr))

    override def visitWithinWindow[I, V, W[+_]](
      expr: WithinWindow[I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opV: OP[W[V]],
      opW: OP[W[Window[V]]],
      opB: OP[W[Boolean]],
    ): H[I, W[Boolean]] = proxy(underlying.visitWithinWindow(expr))

    override def visitZipToShortestHList[I, W[+_], WL <: HList, UL <: HList](
      expr: ZipToShortestHList[I, W, WL, UL, OP],
    )(implicit
      zip: ZipToShortest.Aux[W, WL, OP, UL],
      opO: OP[W[UL]],
    ): H[I, W[UL]] = proxy(underlying.visitZipToShortestHList(expr))
  }

  /**
    * Passes the output of the input expression as the input to the output expression and returns the output of the
    * output expression.
    *
    * You can think of this like the [[Function1.andThen]] method.
    *
    * @param inputExpr the expression to run first
    * @param outputExpr the expression to run with the output of the first expression
    * @param evIOisOI proof that the input expression's output is a subtype of the output expression's expected input
    * @tparam II the input expression's minimum required input type
    * @tparam IO the input expression's most specifically known output type
    * @tparam OI the output expression's minimum required input type
    * @tparam OO the output expression's most specifically known output type
    */
  final case class AndThen[-II, +IO : OP, -OI, +OO : OP, OP[_]](
    inputExpr: Expr[II, IO, OP],
    outputExpr: Expr[OI, OO, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    evIOisOI: IO <:< OI,
  ) extends Expr[II, OO, OP]("andThen") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[II, OO] = v.visitAndThen(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): AndThen[II, IO, OI, OO, OP] =
      copy(debugging = debugging)
  }

  /**
    * Ignores the input and passes the given constant value.
    *
    * You can think of this like the [[scala.Function.const]] function.
    *
    * @param value the constant value to return regardless of the input
    */
  final case class Const[+O : OP, OP[_]](
    value: O,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[Any, O, OP]("const") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, O] = v.visitConst(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Const[O, OP] =
      copy(debugging = debugging)
  }

  // TODO: Rename to CombineWith or BinaryOperation to fit better with Zip2With?
  /**
    * Zips the output of the [[leftExpr]] and [[rightExpr]] (executed in parallel, if supported) and passes
    * the tuple as arguments to the [[operation]] that produces a single output value.
    *
    * This is a fully generic definition for a binary operation, such as `+`, `-`, `/`, `*`, etc. It supports
    * accepting different input types and producing a different output type. This allows the various operators
    * to drive the types based on their needs. For example, you should be able to divide an `Int` by a `Double`
    * to get a `Double`, or divide a `Double` by an `Int` and get back a `Double`. The same applies for other
    * custom defined binary operation relationships.
    *
    * @param leftExpr the left expression
    * @param rightExpr the right expression
    * @param operationName the name of the binary operation
    * @param operation the custom operation function (use this with caution)
    * @param evLOisLI proof that the output of the left expression is a subtype of the operation's expected
    *                 left-side input
    * @param evROisRI proof that the output of the right expression is a subtype of the operation's expected
    *                 right-side input
    * @tparam I the input to both the left and right expressions
    * @tparam LI the operation's left-side input type
    * @tparam LO the output of the left expression
    * @tparam RI the operation's right-side input type
    * @tparam RO the output of the right expression
    * @tparam O the output of the operation
    */
  final case class Combine[-I, -LI, +LO : OP, -RI, +RO : OP, +O : OP, OP[_]](
    leftExpr: Expr[I, LO, OP],
    rightExpr: Expr[I, RO, OP],
    operationName: String,
    operation: (LI, RI) => O,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    evLOisLI: LO <:< LI,
    evROisRI: RO <:< RI,
  ) extends Expr[I, O, OP]("combine") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitCombine(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Combine[I, LI, LO, RI, RO, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Evaluates the given expression nodes and uses `&&` to combine the results from left to right
    * using the provided definition of [[Conjunction]].
    *
    * @param leftExpr the left side of the `AND` operation
    * @param rightExpressions a non-empty vector of right side expressions to reduce with the `AND` operation
    *
    * @tparam B the output of the `AND` operation
    * @tparam W the wrapper type (or effect) over which conjunction is performed
    */
  final case class And[-I, +B, W[+_], OP[_]](
    leftExpr: Expr[I, W[B], OP],
    rightExpressions: NonEmptyVector[Expr[I, W[B], OP]],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    logic: Conjunction[W, B, OP],
    opO: OP[W[B]],
  ) extends Expr[I, W[B], OP]("and") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, W[B]] = v.visitAnd(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): And[I, B, W, OP] =
      copy(debugging = debugging)
  }

  /**
    * Evaluates the given expression nodes and uses `||` to combine the results from left to right
    * using the provided definition of [[Disjunction]].
    *
    * @param leftExpr the left side of the `OR` operation
    * @param rightExpressions a non-empty vector of right side expressions to reduce with the `OR` operation
    *
    * @tparam B the output of the `OR` operation
    * @tparam W the wrapper type (or effect) over which disjunction is performed
    */
  final case class Or[-I, +B, W[+_], OP[_]](
    leftExpr: Expr[I, W[B], OP],
    rightExpressions: NonEmptyVector[Expr[I, W[B], OP]],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    logic: Disjunction[W, B, OP],
    opB: OP[W[B]],
  ) extends Expr[I, W[B], OP]("or") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, W[B]] = v.visitOr(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Or[I, B, W, OP] =
      copy(debugging = debugging)
  }

  /**
    * Negate the output of this expression using the implicit definition of [[Negation]].
    *
    * [[Negation]] is tricky to define properly using constructivist logic. It is defined in this library
    * as the logical negation of the value with no change to the evidence.
    *
    * @param innerExpr the expression to negate
    * @param logic the definition for how to negate the output of this expression.
    *
    * @tparam B the output of the negation operation
    * @tparam W the wrapper type (or effect) over which negation is performed
    */
  final case class Not[-I, +B, W[+_], OP[_]](
    innerExpr: Expr[I, W[B], OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    logic: Negation[W, B, OP],
    opB: OP[W[B]],
  ) extends Expr[I, W[B], OP]("not") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, W[B]] = v.visitNot(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Not[I, B, W, OP] =
      copy(debugging = debugging)
  }

  /**
    * Defines a custom named operation over the input type to produce some defined output type.
    *
    * @note This is an escape hatch for implementing any arbitrary function in native Scala code
    *       with very little limitation in terms of what is allowed. With that in mind, I recommend
    *       great caution when using it as you will lose the benefits of introspectability,
    *       serialization, etc (the things you are using Vapors to provide).
    *
    *       With all that said, though, you probably will need this at some point to do some kind
    *       of transformation that is not supported by the current version of this library. In that
    *       scenario, you can use this expression to get out of a bind. Just be careful to name the
    *       operation appropriately and avoid anything that can be done by other expresison nodes.
    *
    * TODO: Add infix option and other metadata to a special case class type?
    * TODO: Provide the full ExprState so that the operation can utilize the FactTable?
    *
    * @param functionName the name of the custom function
    * @param function the anonymous function to perform the custom action
    */
  final case class CustomFunction[-I, +O : OP, OP[_]](
    functionName: String,
    function: I => O,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("customFunction") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitCustomFunction(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Expr[I, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Applies the given [[conditionExpr]] to every element of the input and returns `true` if there is a single
    * element in the given [[Foldable]] type that satisfies the predicate condition, otherwise `false`.
    *
    * @note this will short-circuit the computation on the first `true` result
    *
    * @param conditionExpr a predicate [[Expr]] that returns either `true` or `false` for every element in the input
    * @tparam C the higher-kinded container type provided as input
    * @tparam A the type of every element of the input
    * @tparam B the condition result type, which must define a way to be viewed as a `Boolean`
    */
  final case class Exists[-C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    combineTrue: NonEmptySeq[B] => B,
    combineFalse: Seq[B] => B,
    shortCircuit: Boolean,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[C[A], B, OP]("exists") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], B] = v.visitExists(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Exists[C, A, B, OP] =
      copy(debugging = debugging)
  }

  /**
    * Applies the given [[conditionExpr]] to every element of the input and returns `true` if every
    * element in the given [[Foldable]] type satisfies the predicate condition or if the input is empty,
    * otherwise `false`.
    *
    * @note this will short-circuit the computation on the first `false` result
    *
    * @param conditionExpr a predicate [[Expr]] that returns either `true` or `false` for every element in the input
    * @tparam C the higher-kinded container type provided as input
    * @tparam A the type of every element of the input
    * @tparam B the condition result type, which must define a way to be viewed as a `Boolean`
    */
  final case class ForAll[-C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    combineTrue: Seq[B] => B,
    combineFalse: NonEmptySeq[B] => B,
    shortCircuit: Boolean,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[C[A], B, OP]("forall") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], B] = v.visitForAll(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): ForAll[C, A, B, OP] =
      copy(debugging = debugging)
  }

  /**
    * Applies the given [[conditionExpr]] to every element of the input only includes the element in the result
    * if it satisfies the predicate condition.
    *
    * @param conditionExpr a predicate [[Expr]] that returns either `true` or `false` for every element in the input
    * @tparam C the higher-kinded container type provided as input
    * @tparam A the type of every element of the input
    * @tparam B the condition result type, which must define a way to be viewed as a `Boolean`
    */
  final case class Filter[C[_], A, +B : ExtractValue.AsBoolean, D[_], OP[_]](
    conditionExpr: Expr[A, B, OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    filter: CollectInto.Filter[C, A, D],
    opO: OP[D[A]],
  ) extends Expr[C[A], D[A], OP]("filter") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], D[A]] = v.visitFilter(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Filter[C, A, B, D, OP] =
      copy(debugging = debugging)
  }

  /**
    * Chain this node to flatten a collection of collections into a lower dimensional collection.
    *
    * This requires a law abiding definition of [[FlatMap]] to be provided. Since `x.flatMap` is
    * equivalent to `x.map().flatten` (if the definition obeys the laws of [[FlatMap]]), this is
    * used to implement both `flatMap` and `flatten`.
    *
    * <h3>How would this support other types of flatten operations?</h3>
    *
    * For most things, we can project into a [[Seq]] of [[Seq]] (either through variance or using a lens)
    * and, since [[Seq]] has a standard definition for [[FlatMap]], we can then flatten down to a
    * lower-dimensional [[Seq]] and convert the result into a more specific collection with [[Select]].
    *
    * However, if we want to support flattening a [[Seq]] of [[Set]]s into a single [[Set]] or other types
    * of "flattening" we would have to create a new operation type for this.
    *
    * While it would be possible to just require a function `C[ C[A] ] => C[A]` and leave it up to the DSL
    * to implement the meaning of "flattening" the types, I think this is preferable to restrict the types
    * of collections that can be flattened and use more specific expression nodes for set operations and only
    * allow `.flatten` and `.flatMap` for types that obey the laws of [[FlatMap]]. This has a clear
    * definition and will probably scale best into the future without over-engineering it now.
    *
    * @tparam C the collection type
    * @tparam A the element type
    */
  final case class Flatten[C[_] : FlatMap, A, OP[_]](
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opO: OP[C[A]],
  ) extends Expr[C[C[A]], C[A], OP]("flatten") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[C[A]], C[A]] = v.visitFlatten(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Flatten[C, A, OP] =
      copy(debugging = debugging)
  }

  /**
    * Passes the input to this expression as its output.
    *
    * You can think of this like the [[identity]] function.
    */
  final case class Identity[I : OP, OP[_]](
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, I, OP]("identity") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, I] = v.visitIdentity(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Identity[I, OP] =
      copy[I, OP](debugging = debugging)
  }

  /**
    * If the output of the [[optionExpr]] is defined, then return it, otherwise evaluate the [[defaultExpr]]
    * and return that.
    *
    * Flattens the [[Option]] to a value of the same type (or more generic).
    *
    * @param optionExpr the input expression that evaluates to an [[Option]]
    * @param defaultExpr an expression to evaluate when the [[Option]] is [[None]]
    */
  final case class GetOrElse[-I, +O : OP, OP[_]](
    optionExpr: Expr[I, Option[O], OP],
    defaultExpr: Expr[I, O, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("getOrElse") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitGetOrElse(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): GetOrElse[I, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Applies the given [[mapExpr]] to every element of the input collection (or effect) of type [[A]] to produce
    * an effect of type [[B]].
    *
    * @param mapExpr the expression to apply to every element of the given collection (or effect)
    * @tparam C the higher-kinded container type provided as input
    * @tparam A the type of every element of the input
    */
  final case class MapEvery[C[_] : Functor, A, B, OP[_]](
    mapExpr: Expr[A, B, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opO: OP[C[B]],
  ) extends Expr[C[A], C[B], OP]("map") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], C[B]] = v.visitMapEvery(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): MapEvery[C, A, B, OP] =
      copy(debugging = debugging)
  }

  /**
    * Select or view a field of the given [[inputExpr]]'s output type using the specified [[VariantLens]].
    *
    * In order to maintain the wrapper type at the appropriate level, this expression node is fairly
    * complex. It relies on type-level computation of the final output type as well as proof that the
    * input expression is returning the correct type of output to apply the lens.
    *
    * @note This would be less complicated if we just required an input of type [[A]], however, then the
    *       only way to construct this node would be to chain a previous operation with [[AndThen]]. This works
    *       fine, however, it means that you can't attach a debugger to this [[Select]] node directly, and since
    *       most of the complication of this expression is how the wrapper type is threaded through the output
    *       of the applied lens, it makes sense to complicate this type to make it easier to debug custom
    *       wrapper type logic.
    * @note The type parameters [[A]] and [[B]] are invariant because of the [[wrapSelected]] function.
    *       While invariant type parameters can be a hassle to work with from the interface user's perspective,
    *       typically the caller only cares about the initial input type [[I]] or the final output type [[O]],
    *       which both utilize variance.
    *
    *       The other option would have been to add an extra type parameter and rely on compiler provided
    *       evidence that the types are compatible when the expression is constructed, but carrying around that
    *       extra type parameter and compiler evidence is probably not worth its weight in code.
    *
    *       In any case where one would want to abstract over <i>specifically</i> the [[Select]] node, they
    *       would have to accept a type parameter for the lens input type, because they cannot just operate
    *       on the types [[Any]] / [[Nothing]]. This is most likely already the case, as those types are not
    *       very useful, and if you don't care about the type, you can use the `_` wildcard.
    *
    *       It is a bit easier to add variance to a public interface than to take it away, so I opted to
    *       make the input type parameters [[A]] and [[B]] invariant, rather than split them into two types each.
    * @param inputExpr  the expression to run before applying the [[lens]] and [[wrapSelected]] on the result
    * @param wrapSelected a function that defines how to wrap the result of applying the [[lens]].
    *                   If it is a higher-kinded Functor, wrap every element, otherwise wrap the single value directly.
    * @param lens       a lens from the input type to some selected field
    * @tparam A the output of the input expression and the input to the lens
    * @tparam B the output of the lens
    * @tparam O the final computed output type with the wrapper type applied at the appropriate level
    *           (computed using the [[dsl.SelectOutputType]] implicit)
    */
  final case class Select[-I, A, B, +O : OP, OP[_]](
    inputExpr: Expr[I, A, OP],
    lens: VariantLens[A, B],
    wrapSelected: (A, B) => O,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("select") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitSelect(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Select[I, A, B, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Takes a traversable sequence of expressions that accept the same input type and return the same output type
    * and creates an expression that takes the same input type and returns the same traversable type sequence of
    * output values.
    *
    * @param expressions the sequence of expressions to evaluate in order to create the same sequence of results
    * @tparam C the traversable collection
    */
  final case class Sequence[+C[+_] : Traverse, -I, +O, OP[_]](
    expressions: C[Expr[I, O, OP]],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opO: OP[C[O]],
  ) extends Expr[I, C[O], OP]("sequence") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, C[O]] = v.visitSequence(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Sequence[C, I, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * As a general-purpose looping construct that can implement many different operations, the fold left
    * operation is used to apply a function continuously to an accumulating value (the "left" side) and
    * every element of a [[Foldable]] collection (passed in as the "right" side of the function).
    *
    * @note The fold operation is the swiss army knife of functional programming. It can be used to implement
    *       all kinds of operations. This utility comes at the possible cost of obfuscating the goal of the
    *       operation. You should try to favor more direct approaches when possible, but this exists as both
    *       an escape hatch for some more complex operations as well as the best way to implement a loop in
    *       a declarative way.
    *
    * @param inputExpr an expression that produces a [[Foldable]] output
    * @param initExpr an expression that produces an initial value for the accumulator (the "left" side)
    * @param foldExpr an expression that operates on a tuple of the accumulated ("left" side) and the next
    *                 element of the [[Foldable]] inputExpr result ("right" side) to produce a final output
    *                 of the accumulated value.
    *
    * @tparam C the [[Foldable]] collection type
    * @tparam A the element type
    * @tparam O the accumulator type, initial value, and output type
    */
  final case class FoldLeft[-I, +C[_] : Foldable, A, O : OP, OP[_]](
    inputExpr: Expr[I, C[A], OP],
    initExpr: Expr[I, O, OP],
    foldExpr: Expr[(O, A), O, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("foldLeft") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitFoldLeft(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): FoldLeft[I, C, A, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Grabs all facts of the given [[FactTypeSet]]'s type and returns them as output.
    *
    * @param factTypeSet a set of fact types that all share the same type (can be one or more types)
    * @tparam T the type of value for the matching facts
    */
  final case class ValuesOfType[T, +O, OP[_]](
    factTypeSet: FactTypeSet[T],
    transform: TypedFact[T] => O,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opTs: OP[Seq[O]],
  ) extends Expr[Any, Seq[O], OP]("valuesOfType") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, Seq[O]] = v.visitValuesOfType(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): ValuesOfType[T, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Computes two expressions (possibly in parallel) and then checks if the resulting values are equal
    * using the provided [[EqualComparable]].
    *
    * @tparam V the value type to compare
    * @tparam W the wrapper type constructor for the context in which the values are compared
    */
  final case class IsEqual[-I, +V : OP, W[+_], OP[_]](
    leftExpr: Expr[I, W[V], OP],
    rightExpr: Expr[I, W[V], OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    eq: EqualComparable[W, V, OP],
    opV: OP[W[V]],
    opB: OP[W[Boolean]],
  ) extends Expr[I, W[Boolean], OP]("isEqual") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, W[Boolean]] = v.visitIsEqual(this)
    override def withDebugging(debugging: Debugging[Nothing, Nothing]): IsEqual[I, V, W, OP] =
      copy(debugging = debugging)
  }

  /**
    * Compare the size of the input type to the given [[comparedTo]] expression.
    *
    * See [[SizeComparison]] to choose how to compare the size.
    *
    * @note It is not possible to choose how you want to compare things using an expression
    *       because the [[SizeComparison]] type is not a primitive value of the language,
    *       but rather, just a syntactic component.
    *
    * @param comparison the type of comparison to perform
    * @param comparedTo the integral numeric value to compare the size of input to
    * @param compare the definition of how to compare the size of input to the numeric value
    *
    * @tparam I the input value type
    * @tparam N a numeric integral viewable type
    * @tparam B a boolean-like type to be returned as output
    */
  final case class SizeIs[-I, N : ExtractValue[*, Int], B : ExtractValue.AsBoolean : OP, OP[_]](
    comparison: SizeComparison,
    comparedTo: Expr[I, N, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    compare: SizeComparable[I, N, B],
  ) extends Expr[I, B, OP]("sizeIs") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, B] = v.visitSizeIs(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): SizeIs[I, N, B, OP] =
      copy(debugging = debugging)
  }

  /**
    * Return a slice of the input collection based on a given relative [[SliceRange]].
    *
    * This may change the type of collection from [[C]] to [[D]] in the process.
    *
    * @see [[CollectInto]] for more details on converting collection types.
    *
    * @param range a [[SliceRange.Relative]] to be applied to a collection of type [[C]]
    * @param filter the definition for how to filter the collection from type [[C]] to type [[D]]
    * @tparam C the input collection type
    * @tparam A the collection element type
    * @tparam D the resulting collection type
    */
  final case class Slice[C[_] : Traverse, A, D[_], OP[_]](
    range: SliceRange.Relative,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    filter: CollectInto.Filter[C, A, D],
    opO: OP[D[A]],
  ) extends Expr[C[A], D[A], OP]("slice") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], D[A]] = v.visitSlice(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Slice[C, A, D, OP] =
      copy(debugging = debugging)
  }

  /**
    * Applies the given [[Sortable]] for the elements of the input collection (or effect) of type [[A]] to produce
    * a collection of the same type.
    *
    * @tparam C the higher-kinded container type provided as input
    * @tparam A the type of every element of the input (and output)
    */
  final case class Sorted[C[_], A, OP[_]](
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    sortable: Sortable[C, A],
    opAs: OP[C[A]],
  ) extends Expr[C[A], C[A], OP]("sorted") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], C[A]] = v.visitSorted(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Sorted[C, A, OP] =
      copy(debugging = debugging)
  }

  /**
    * A simplified trait for [[Define]] with less type parameters, so that it is easier to refer to.
    *
    * It produces a Seq of [[Fact]], rather than [[TypedFact]]s, since you don't need the type information
    * until you extract the facts from the [[FactTable]].
    */
  sealed trait Definition[-I, OP[_]] extends Expr[I, Seq[Fact], OP]

  /**
    * Defines an expression that produces values of the given [[factType]].
    *
    * @param factType the [[FactType]] defining the type of values needed to produce instances of these facts
    * @param defnExpr the expression used to produce the facts
    *
    * @tparam C a [[Foldable]] collection type that can be used to fold the resulting [[Fact]]s into a single List
    * @tparam T the type of values used to produce [[TypedFact]] instances of the given [[factType]]
    */
  final case class Define[-I, +C[_] : Foldable, T, OP[_]](
    factType: FactType[T],
    defnExpr: Expr[I, C[T], OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opF: OP[Seq[TypedFact[T]]],
  ) extends Expr[I, Seq[TypedFact[T]], OP]("define")
    with Definition[I, OP] {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, Seq[TypedFact[T]]] = v.visitDefine(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Define[I, C, T, OP] =
      copy(debugging = debugging)
  }

  /**
    * Adds the facts produced by the given definitions to the [[FactTable]] before computing the given [[thenExpr]].
    *
    * @note While this accepts a [[Seq]] of definitions, this only affects the order in which the facts are produced
    *       to ensure consistent behavior. All facts will be available before evaluating the [[thenExpr]].
    *
    * @param definitions a sequence of definitions to compute to create the facts to add to the [[FactTable]]
    * @param thenExpr the expression to compute after the facts produced by the definitions are all available
    *                 in the [[FactTable]]
    */
  final case class UsingDefinitions[-I, +O : OP, OP[_]](
    definitions: Seq[Definition[I, OP]],
    thenExpr: Expr[I, O, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("usingDefinitions") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitUsingDefinitions(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): UsingDefinitions[I, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * A container for a condition and an expression to compute if the condition is met.
    *
    * @see [[Expr.When]] for usage.
    *
    * @param whenExpr a conditional expression that guards the resulting [[thenExpr]]
    * @param thenExpr an expression to compute the result if the [[whenExpr]] returns true
    */
  final case class ConditionBranch[-I, +B : ExtractValue.AsBoolean, +O, OP[_]](
    whenExpr: Expr[I, B, OP],
    thenExpr: Expr[I, O, OP],
  )

  /**
    * A branching if [ / elif ...] / else conditional operation.
    *
    * @param conditionBranches all of the branches that are guarded by condition expressions
    * @param defaultExpr the expression to run if none of the branch conditions matches
    * @tparam B a Boolean-like type that is used to determine if a branch condition is matched should run
    */
  final case class When[-I, +B : ExtractValue.AsBoolean, +O : OP, OP[_]](
    conditionBranches: NonEmptySeq[ConditionBranch[I, B, O, OP]],
    defaultExpr: Expr[I, O, OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("when") {
    lazy val whenExpressions: NonEmptySeq[Expr[I, B, OP]] = conditionBranches.map(_.whenExpr)
    lazy val thenExpressions: NonEmptySeq[Expr[I, O, OP]] = conditionBranches.map(_.thenExpr) :+ defaultExpr
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitWhen(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): When[I, B, O, OP] =
      copy(debugging = debugging)
  }

  /**
    * Evaluates the [[valueExpr]] to get an effect-wrapped value, evaluates the [[windowExpr]] to get the same
    * effect wrapped window, then checks if the value produced is within the window produced.
    *
    * @param valueExpr an expression returning a value from the starting input
    * @param windowExpr an expression returning a window from the starting input
    * @tparam I the input value type
    * @tparam V the window value type
    * @tparam W the wrapper type, used to carry along metadata beyond just true / false.
    *           This can also be an effect type, but it must be covariant on its inner type.
    */
  final case class WithinWindow[-I, +V, W[+_], OP[_]](
    valueExpr: Expr[I, W[V], OP],
    windowExpr: Expr[I, W[Window[V]], OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    comparison: WindowComparable[W, OP],
    opV: OP[W[V]],
    opW: OP[W[Window[V]]],
    opB: OP[W[Boolean]],
  ) extends Expr[I, W[Boolean], OP]("withinWindow") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, W[Boolean]] = v.visitWithinWindow(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): WithinWindow[I, V, W, OP] =
      copy(debugging = debugging)
  }

  /**
    * Apply a serializable [[ExprConverter]] function to the input type [[I]] to view it as a value of type [[O]].
    *
    * This can be used to view an [[HList]] as a product type with the number and type of elements.
    *
    * @param converter the serializable [[ExprConverter]] used to view the input type [[I]] as [[O]]
    */
  final case class Convert[-I, +O : OP, OP[_]](
    converter: ExprConverter[I, O],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("convert") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitConvert(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Convert[I, O, OP] =
      copy(debugging = debugging)
  }

  // TODO: Is there a way to combine this with the Convert operation?
  //       Maybe the converter can take the visitor as an argument to its function?
  final case class ToHList[-I, +L <: HList : OP, OP[_]](
    exprHList: ExprHList[I, L, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    toHL: ConvertToHList[L],
  ) extends Expr[I, L, OP]("toHList") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, L] = v.visitToHList(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): ToHList[I, L, OP] =
      copy(debugging = debugging)
  }

  /**
    * Zip the output of the elements of all the [[Expr]] nodes of the given [[ExprHList]] up to the
    * length of the shortest given collection.
    *
    * @see [[ZipToShortest]] for more details
    *
    * @param exprHList the fixed-size heterogeneous list of [[Expr]] nodes
    * @param zip the definition of how to zip the output elements of the expressions
    *
    * @tparam I the input value type
    * @tparam W the wrapper type (with the embedded collection type)
    * @tparam WL an [[HList]] containing all the wrapped output types of all the [[Expr]] nodes embedded in
    *            the [[exprHList]]
    * @tparam UL an [[HList]] containing all the unwrapped output types of all the [[Expr]] nodes embedded
    *            in the [[exprHList]]
    */
  final case class ZipToShortestHList[-I, W[+_], +WL <: HList, +UL <: HList, OP[_]](
    exprHList: ExprHList[I, WL, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    zip: ZipToShortest.Aux[W, WL, OP, UL],
    opO: OP[W[UL]],
  ) extends Expr[I, W[UL], OP]("zipToShortest") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, W[UL]] = v.visitZipToShortestHList(this)
    override private[v1] def withDebugging(
      debugging: Debugging[Nothing, Nothing],
    ): ZipToShortestHList[I, W, WL, UL, OP] =
      copy(debugging = debugging)
  }
}

package com.rallyhealth.vapors.v1

package algebra

import data.{ExprState, FactTypeSet, TypedFact, Window}
import debug.{DebugArgs, Debugging, NoDebugging}
import lens.VariantLens
import logic.Negation
import math.Add

import cats.data.NonEmptyList
import cats.{Foldable, Functor}

import scala.annotation.nowarn

/**
  * The root trait of all expression nodes.
  *
  * An expression can be interpreted to produce a function, a JSON object, or really, anything you want!
  *
  * The expression node is contravariant on the input
  *
  * The expression defines a set of useful operations (modeled after the Scala collections library) that
  * contain enough information to be serializable.
  *
  * @tparam I the input value type
  * @tparam O the output value type
  */
sealed abstract class Expr[-I, +O : OP, OP[_]](val name: String) {

  /**
    * Holds any [[Debugging]] hook to be run while running this expression.
    *
    * You should favor using the `.debug` extension method provided by the [[Expr.DebugSyntax]]
    *
    * @note Only one hook can be attached to an expression. The last one attached wins.
    *       TODO: Remove this limitation by chaining debug functions together.
    *
    * @note As a top-level [[Expr]] method, this is not very useful, as you cannot invoke it, however, you can
    *       print or inspect it for information at runtime; and placing it here prevents me from forgetting to
    *       add this to subclasses. The types are refined when attaching the debug hooks using the [[DebugArgs]]
    *       compiler trick.
    */
  def debugging: Debugging[Nothing, Nothing]

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
    */
  def withDebugging(debugging: Debugging[Any, Any]): Expr[I, O, OP]

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

  def andThen[OI >: O, OO : OP](that: Expr[OI, OO, OP]): Expr.AndThen[I, O, OI, OO, OP] =
    Expr.AndThen(this, that)

  def +[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    add: Add[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "add", add.combine(_, _): @nowarn)
  }

  def unary_![RO >: O](
    implicit
    negation: Negation[RO],
    opA: OP[RO],
  ): Expr.Not[I, RO, OP] =
    Expr.Not[I, RO, OP](this)

  // TODO: Match on self and convert to string recursively as lazy val
  override def toString: String = name
}

object Expr {

  final type AnyWith[OP[_]] = Expr[Nothing, Any, OP]

  implicit def debugAnyExpr[I, O, OP[_]](expr: Expr[I, O, OP]): DebugSyntax[Expr[I, O, OP], Any, Any, OP] =
    new DebugSyntax(hook => expr.withDebugging(Debugging(hook)))

  implicit def debugExpr[E <: Expr.AnyWith[OP], OP[_]](
    expr: E,
  )(implicit
    debugArgs: DebugArgs[E, OP],
  ): DebugSyntax[E, debugArgs.In, debugArgs.Out, OP] =
    new DebugSyntax(debugArgs.attachHook(expr, _))

  /**
    * Uses the fixed types supplied by the [[DebugArgs]] compiler trick to provide better type inference
    * when attaching a hook.
    *
    * @param attachHook a captured function that will attach a given hook function to the specific [[Expr]] type
    * @tparam E The specific [[Expr]] subclass used to compute the debug types
    * @tparam DI The `Debugging Input` type (this doesn't always need to match the actual input to the [[Expr]])
    *            Typically, this is a tuple of all of the input types of any sub-expressions preceded by the
    *            input to the given expression.
    * @tparam DO The `Debugging Output` type (this is always the same as the [[Expr]]'s output type)
    * @tparam OP The `Output Parameter` (captured at the definition site for every output type in the expression tree)
    */
  final class DebugSyntax[E <: Expr.AnyWith[OP], DI, DO, OP[_]](
    private val attachHook: (ExprState[DI, DO] => Unit) => E,
  ) extends AnyVal {

    /**
      * Attach the given function to run when the expression is being interpreted. It allows you to intercept
      * the computation and inspect the current state, but it does not allow you to alter it.
      *
      * TODO: The ability to alter the local state might also be helpful for debugging purposes, but it should
      *       be something that you can disable for a more "production secure" setup. That said... maybe the
      *       entire capability of attaching a side-effecting function should be something you can turn off.
      *       Either way, we should always support this `ExprState => Unit` interface, so it would have to be
      *       a separate method anyway.
      */
    def debug(hook: ExprState[DI, DO] => Unit): E = attachHook(hook)
  }

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
    * @tparam ~> an infix type alias for the higher-kinded result type of this visitor
    * @tparam OP `Output Parameter` (captured at the definition site for every output type in the expression tree)
    */
  trait Visitor[~>[-_, +_], OP[_]] {

    def visitAnd[I](expr: And[I, OP])(implicit opO: OP[Boolean]): I ~> Boolean

    def visitAndThen[II, IO : OP, OI, OO : OP](expr: AndThen[II, IO, OI, OO, OP])(implicit evBI: IO <:< OI): II ~> OO

    def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): I ~> O

    def visitConst[O : OP](expr: Const[O, OP]): Any ~> O

    def visitCustomFunction[I, O : OP](expr: CustomFunction[I, O, OP]): I ~> O

    def visitExists[C[_] : Foldable, A, B : OP](expr: Exists[C, A, B, OP]): C[A] ~> B

    def visitForAll[C[_] : Foldable, A, B : OP](expr: ForAll[C, A, B, OP]): C[A] ~> B

    def visitIdentity[I : OP](expr: Identity[I, OP]): I ~> I

    def visitMapEvery[C[_] : Functor, A, B](expr: MapEvery[C, A, B, OP])(implicit opO: OP[C[B]]): C[A] ~> C[B]

    def visitNot[I, O : Negation : OP](expr: Not[I, O, OP]): I ~> O

    def visitOr[I](expr: Or[I, OP])(implicit evO: OP[Boolean]): I ~> Boolean

    def visitSelect[I, O : OP](expr: Select[I, O, OP]): I ~> O

    def visitValuesOfType[T, O](expr: ValuesOfType[T, O, OP])(implicit opTs: OP[Seq[O]]): Any ~> Seq[O]

    def visitWithinWindow[I, V : OP, F[+_]](
      expr: WithinWindow[I, V, F, OP],
    )(implicit
      comparison: WindowComparable[F, OP],
      opB: OP[F[Boolean]],
    ): I ~> F[Boolean]
  }

  final case class And[-I, OP[_]](
    leftExpr: Expr[I, Boolean, OP],
    rightExpr: Expr[I, Boolean, OP],
    debugging: Debugging[I, Boolean] = NoDebugging,
  )(implicit
    opO: OP[Boolean],
  ) extends Expr[I, Boolean, OP]("and") {
    override def withDebugging(debugging: Debugging[Any, Any]): Expr[I, Boolean, OP] = copy(debugging = debugging)
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, Boolean] = v.visitAnd(this)
  }

  final case class Or[-I, OP[_]](
    leftExpr: Expr[I, Boolean, OP],
    rightExpr: Expr[I, Boolean, OP],
    debugging: Debugging[I, Boolean] = NoDebugging,
  )(implicit
    opO: OP[Boolean],
  ) extends Expr[I, Boolean, OP]("or") {
    override def withDebugging(debugging: Debugging[Any, Any]): Expr[I, Boolean, OP] = copy(debugging = debugging)
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, Boolean] = v.visitOr(this)
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
    debugging: Debugging[(II, OI), Any] = NoDebugging,
  )(implicit
    evIOisOI: IO <:< OI,
  ) extends Expr[II, OO, OP]("andThen") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[II, OO] = v.visitAndThen(this)
    override def withDebugging(debugging: Debugging[Any, Any]): AndThen[II, IO, OI, OO, OP] =
      copy(debugging = debugging)
  }

  /**
    * Ignores the input and passes the given constant value.
    *
    * You can think of this like the [[scala.Function.const]] function.
    */
  final case class Const[+O : OP, OP[_]](
    value: O,
    debugging: Debugging[Any, Any] = NoDebugging,
  ) extends Expr[Any, O, OP]("const") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, O] = v.visitConst(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Const[O, OP] = copy(debugging = debugging)
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
    debugging: Debugging[(I, LI, RI), Any] = NoDebugging,
  )(implicit
    evLOisLI: LO <:< LI,
    evROisRI: RO <:< RI,
  ) extends Expr[I, O, OP]("combine") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitCombine(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Combine[I, LI, LO, RI, RO, O, OP] =
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
    debugging: Debugging[I, Any] = NoDebugging,
  ) extends Expr[I, O, OP]("customFunction") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitCustomFunction(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Expr[I, O, OP] = copy(debugging = debugging)
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
    * @tparam B the output type, which must define a way to be viewed as a Boolean
    */
  final case class Exists[C[_] : Foldable, A, B : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    asBoolean: B => Boolean, // TODO: Should this use the ExtractBoolean constraint?
    combineTrue: NonEmptyList[B] => B,
    combineFalse: List[B] => B,
    shortCircuit: Boolean,
    debugging: Debugging[C[A], B] = NoDebugging,
  ) extends Expr[C[A], B, OP]("exists") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], B] = v.visitExists(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Exists[C, A, B, OP] = copy(debugging = debugging)
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
    * @tparam B the output type, which must define a way to be viewed as a Boolean
    */
  final case class ForAll[C[_] : Foldable, A, B : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    asBoolean: B => Boolean, // TODO: Should this use the ExtractBoolean constraint?
    combineTrue: List[B] => B,
    combineFalse: NonEmptyList[B] => B,
    shortCircuit: Boolean,
    debugging: Debugging[C[A], B] = NoDebugging,
  ) extends Expr[C[A], B, OP]("forall") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], B] = v.visitForAll(this)
    override def withDebugging(debugging: Debugging[Any, Any]): ForAll[C, A, B, OP] = copy(debugging = debugging)
  }

  /**
    * Passes the input to this expression as its output.
    *
    * You can think of this like the [[identity]] function.
    */
  final case class Identity[I : OP, OP[_]](debugging: Debugging[I, I] = NoDebugging)
    extends Expr[I, I, OP]("identity") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, I] = v.visitIdentity(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Identity[I, OP] =
      copy[I, OP](debugging = debugging)
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
    debugging: Debugging[C[A], C[B]] = NoDebugging,
  )(implicit
    opO: OP[C[B]],
  ) extends Expr[C[A], C[B], OP]("map") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], C[B]] = v.visitMapEvery(this)
    override def withDebugging(debugging: Debugging[Any, Any]): MapEvery[C, A, B, OP] = copy(debugging = debugging)
  }

  final case class Not[-I, +O : Negation : OP, OP[_]](
    innerExpr: Expr[I, O, OP],
    debugging: Debugging[Any, Any] = NoDebugging,
  ) extends Expr[I, O, OP]("not") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitNot(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Not[I, O, OP] = copy(debugging = debugging)
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
    debugging: Debugging[Any, Seq[Any]] = NoDebugging,
  )(implicit
    opTs: OP[Seq[O]],
  ) extends Expr[Any, Seq[O], OP]("valuesOfType") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, Seq[O]] = v.visitValuesOfType(this)
    override def withDebugging(debugging: Debugging[Any, Any]): ValuesOfType[T, O, OP] = copy(debugging = debugging)
  }

  final case class WithinWindow[-I, +V : OP, F[+_], OP[_]](
    valueExpr: Expr[I, F[V], OP],
    windowExpr: Expr[I, F[Window[V]], OP],
    debugging: Debugging[Any, F[Boolean]] = NoDebugging,
  )(implicit
    comparison: WindowComparable[F, OP],
    opB: OP[F[Boolean]],
  ) extends Expr[I, F[Boolean], OP]("withinWindow") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[Boolean]] = v.visitWithinWindow(this)
    override def withDebugging(debugging: Debugging[Any, Any]): WithinWindow[I, V, F, OP] =
      copy(debugging = debugging)
  }

  final case class Select[-I, +O : OP, OP[_]](
    lens: VariantLens[I, O],
    debugging: Debugging[I, Any] = NoDebugging,
  ) extends Expr[I, O, OP]("select") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitSelect(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Select[I, O, OP] = copy(debugging = debugging)
  }
}

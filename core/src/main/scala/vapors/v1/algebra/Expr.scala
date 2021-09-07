package com.rallyhealth

package vapors.v1.algebra

import vapors.v1.data.{ExprState, FactTypeSet}
import vapors.v1.debug.{DebugArgs, Debugging, NoDebugging}
import vapors.v1.math.Add

import cats.Foldable

/**
  * Required features:
  *
  * - Intermediate caching
  * - Stack-free computation (i.e. conversion to cats / zio)
  * - Interpretation
  * - Reflection
  * - Support for flatMap
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

  def +[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    add: Add[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, add.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "add", add.combine(_, _))
  }

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

    def visitAndThen[II, IO : OP, OI, OO : OP](expr: AndThen[II, IO, OI, OO, OP])(implicit evBI: IO <:< OI): II ~> OO

    def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): I ~> O

    def visitConst[O : OP](expr: Const[O, OP]): Any ~> O

    def visitExists[C[_] : Foldable, E](
      expr: Exists[C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): C[E] ~> Boolean

    def visitForAll[C[_] : Foldable, E](
      expr: ForAll[C, E, OP],
    )(implicit
      opO: OP[Boolean],
    ): C[E] ~> Boolean

    def visitIdentity[I, O : OP](expr: Identity[I, O, OP])(implicit evO: I <:< O): I ~> O

    def visitValuesOfType[T](expr: ValuesOfType[T, OP])(implicit opTs: OP[Seq[T]]): Any ~> Seq[T]
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
    * Passes the input to this expression as its output.
    *
    * You can think of this like the [[identity]] function.
    *
    * @param evIisO proof that the input type is a subtype of the output type
    *               (they should be the same type, but because of variance, we have to supply the proof at the call
    *               site that the expected input of the expression this is supplied to is a subtype of the input
    *               to this expression).
    *               TODO: Although I tried various times to remove the second type parameter and make this invariant,
    *                     I couldn't seem to get it to work. Maybe there are still some type tricks I can employ in
    *                     the DSL layer that can avoid complicating this type?
    */
  final case class Identity[-I, +O : OP, OP[_]](
    debugging: Debugging[I, I] = NoDebugging,
  )(implicit
    evIisO: I <:< O,
  ) extends Expr[I, O, OP]("identity") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitIdentity(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Identity[I, O, OP] =
      copy[I, O, OP](debugging = debugging)
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
    * Applies the given [[conditionExpr]] to every element of the input and returns `true` if there is a single
    * element in the given [[Foldable]] type that satisfies the predicate condition, otherwise `false`.
    *
    * @note this will short-circuit the computation on the first `true` result
    *
    * @param conditionExpr a predicate [[Expr]] that returns either `true` or `false` for every element in the input
    * @tparam C the higher-kinded container type provided as input
    * @tparam E the type of every element of the input
    */
  final case class Exists[C[_] : Foldable, E, OP[_]](
    conditionExpr: Expr[E, Boolean, OP],
    debugging: Debugging[C[E], Boolean] = NoDebugging,
  )(implicit
    opO: OP[Boolean],
  ) extends Expr[C[E], Boolean, OP]("exists") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[E], Boolean] = v.visitExists(this)
    override def withDebugging(debugging: Debugging[Any, Any]): Exists[C, E, OP] = copy(debugging = debugging)
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
    * @tparam E the type of every element of the input
    */
  final case class ForAll[C[_] : Foldable, E, OP[_]](
    conditionExpr: Expr[E, Boolean, OP],
    debugging: Debugging[C[E], Boolean] = NoDebugging,
  )(implicit
    opO: OP[Boolean],
  ) extends Expr[C[E], Boolean, OP]("forall") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[E], Boolean] = v.visitForAll(this)
    override def withDebugging(debugging: Debugging[Any, Any]): ForAll[C, E, OP] = copy(debugging = debugging)
  }

  /**
    * Grabs all facts of the given [[FactTypeSet]]'s type and returns them as output.
    *
    * @param factTypeSet a set of fact types that all share the same type (can be one or more types)
    * @tparam T the type of value for the matching facts
    */
  final case class ValuesOfType[T, OP[_]](
    factTypeSet: FactTypeSet[T],
    debugging: Debugging[Any, Seq[T]] = NoDebugging,
  )(implicit
    opTs: OP[Seq[T]],
  ) extends Expr[Any, Seq[T], OP]("valuesOfType") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[Any, Seq[T]] = v.visitValuesOfType(this)
    override def withDebugging(debugging: Debugging[Any, Any]): ValuesOfType[T, OP] = copy(debugging = debugging)
  }
}

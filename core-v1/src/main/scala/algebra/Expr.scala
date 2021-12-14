package com.rallyhealth.vapors.v1

package algebra

import cats.data.{NonEmptyList, NonEmptyVector}
import cats.{Foldable, Functor}
import data.{ExtractValue, FactTypeSet, TypedFact, Window}
import debug.{DebugArgs, Debugging, NoDebugging}
import lens.{DataPath, VariantLens}
import logic.{Conjunction, Disjunction, Negation}
import math.Add

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
    * @param add the type-level definition of how to add this type of output to that type of element
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
    new CombineHolder(this, that, "add", add.combine(_, _): @nowarn)
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
    * @tparam OP `Output Parameter` (captured at the definition site for every output type in the expression tree)
    */
  trait Visitor[~:>[-_, +_], OP[_]] {

    def visitAnd[I, B, W[+_]](
      expr: And[I, B, W, OP],
    )(implicit
      logic: Conjunction[W, B, OP],
      opB: OP[W[B]],
    ): I ~:> W[B]

    def visitAndThen[II, IO : OP, OI, OO : OP](expr: AndThen[II, IO, OI, OO, OP])(implicit evBI: IO <:< OI): II ~:> OO

    def visitCombine[I, LI, LO : OP, RI, RO : OP, O : OP](
      expr: Combine[I, LI, LO, RI, RO, O, OP],
    )(implicit
      evLOisLI: LO <:< LI,
      evROisRI: RO <:< RI,
    ): I ~:> O

    def visitConst[O : OP](expr: Const[O, OP]): Any ~:> O

    def visitCustomFunction[I, O : OP](expr: CustomFunction[I, O, OP]): I ~:> O

    def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](expr: Exists[C, A, B, OP]): C[A] ~:> B

    def visitForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](expr: ForAll[C, A, B, OP]): C[A] ~:> B

    def visitIdentity[I : OP](expr: Identity[I, OP]): I ~:> I

    def visitIsEqual[I, V, W[+_]](
      expr: IsEqual[I, V, W, OP],
    )(implicit
      eq: EqualComparable[W, V, OP],
      opV: OP[W[V]],
      opO: OP[W[Boolean]],
    ): I ~:> W[Boolean]

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

    def visitValuesOfType[T, O](expr: ValuesOfType[T, O, OP])(implicit opTs: OP[Seq[O]]): Any ~:> Seq[O]

    def visitWithinWindow[I, V, W[+_]](
      expr: WithinWindow[I, V, W, OP],
    )(implicit
      comparison: WindowComparable[W, OP],
      opV: OP[W[V]],
      opW: OP[W[Window[V]]],
      opB: OP[W[Boolean]],
    ): I ~:> W[Boolean]
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
    * @tparam B the output type, which must define a way to be viewed as a Boolean
    */
  final case class Exists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    combineTrue: NonEmptyList[B] => B,
    combineFalse: List[B] => B,
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
    * @tparam B the output type, which must define a way to be viewed as a Boolean
    */
  final case class ForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    combineTrue: List[B] => B,
    combineFalse: NonEmptyList[B] => B,
    shortCircuit: Boolean,
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[C[A], B, OP]("forall") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], B] = v.visitForAll(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): ForAll[C, A, B, OP] =
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
}

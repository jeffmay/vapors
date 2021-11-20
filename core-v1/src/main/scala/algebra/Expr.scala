package com.rallyhealth.vapors.v1

package algebra

import data.{ExtractValue, FactTypeSet, TypedFact, Window}
import debug.{DebugArgs, Debugging, NoDebugging}
import lens.VariantLens
import logic.{Conjunction, Disjunction, Negation}
import math._

import cats.data.NonEmptyList
import cats.{Foldable, Functor, FunctorFilter, Semigroupal}
import com.rallyhealth.vapors.v1.data.ExtractValue.AsBoolean
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
  * @tparam OP the custom output parameter type constructor (defined by the imported DSL)
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
    * @param sub the type-level definition of how to subtract that type of output from this type of output
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
    * @param mult the type-level definition of how to multiply this type of output to that type of element
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
    * @param div the type-level definition of how to divide this type of output by that type of output
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

  def ^[CI <: I, LI >: O, RI >: RO, RO <: RI : OP](
    that: Expr[CI, RO, OP],
  )(implicit
    pow: Power[LI, RI],
  ): CombineHolder[CI, LI, O, RI, RO, pow.Out, OP] = {
    // can't eta-expand a dependent object function, the (_, _) is required
    new CombineHolder(this, that, "pow", pow.pow(_, _): @nowarn)
  }

  def selectWith[OI >: O, OO : OP](lens: VariantLens[OI, OO]): Expr.AndThen[I, O, OI, OO, OP] =
    Expr.AndThen(this, Expr.Select(lens))

//  /**
//    * Negate the output of this expression using the implicit definition of [[Negation]].
//    *
//    * @param negation the definition for how to negate the output of this expression.
//    */
//  def unary_![RO >: O](
//    implicit
//    negation: Negation[RO],
//    opA: OP[RO],
//  ): Expr.Not[I, RO, OP] =
//    Expr.Not[I, RO, OP](this)

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

    def visitAnd[I, B, F[+_]](
      expr: And[I, B, F, OP],
    )(implicit
      logic: Conjunction[F, B, OP],
      opB: OP[F[B]],
    ): I ~:> F[B]

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

    def visitConcatToHList[I, F[+_], WL <: HList : OP, UL <: HList](expr: ConcatToHList[I, F, WL, UL, OP]): I ~:> WL

    def visitConst[O : OP](expr: Const[O, OP]): Any ~:> O

    def visitConvert[I, O : OP](expr: Convert[I, O, OP]): I ~:> O

    def visitCustomFunction[I, O : OP](expr: CustomFunction[I, O, OP]): I ~:> O

    def visitExists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](expr: Exists[C, A, B, OP]): C[A] ~:> B

    def visitFilter[C[_] : FunctorFilter, A, B : ExtractValue.AsBoolean : OP](
      expr: Filter[C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): C[A] ~:> C[A]

    def visitForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP](expr: ForAll[C, A, B, OP]): C[A] ~:> B

    def visitIdentity[I : OP](expr: Identity[I, OP]): I ~:> I

    def visitIsEqual[I, V, F[+_]](expr: IsEqual[I, V, F, OP])(implicit eq: EqualComparable[F, V]): I ~:> F[Boolean]

    def visitMapEvery[C[_] : Functor, A, B](expr: MapEvery[C, A, B, OP])(implicit opO: OP[C[B]]): C[A] ~:> C[B]

    def visitNot[I, B, F[+_]](
      expr: Not[I, B, F, OP],
    )(implicit
      logic: Negation[F, B, OP],
      opB: OP[F[B]],
    ): I ~:> F[B]

    def visitOr[I, B, F[+_]](
      expr: Or[I, B, F, OP],
    )(implicit
      logic: Disjunction[F, B, OP],
      opO: OP[F[B]],
    ): I ~:> F[B]

    def visitSelect[I, O : OP](expr: Select[I, O, OP]): I ~:> O

    def visitValuesOfType[T, O](expr: ValuesOfType[T, O, OP])(implicit opTs: OP[Seq[O]]): Any ~:> Seq[O]

    def visitWithinWindow[I, V, F[+_]](
      expr: WithinWindow[I, V, F, OP],
    )(implicit
      comparison: WindowComparable[F, OP],
      opV: OP[F[V]],
      opW: OP[F[Window[V]]],
      opB: OP[F[Boolean]],
    ): I ~:> F[Boolean]

    def visitZipToHList[I, F[+_] : Functor : Semigroupal, WL <: HList, UL <: HList](
      expr: Expr.ZipToHList[I, F, WL, UL, OP],
    )(implicit
      opO: OP[F[UL]],
    ): I ~:> F[UL]
  }

  abstract class ProxyVisitor[G[-_, +_], H[-_, +_], OP[_]](protected val underlying: Visitor[G, OP])
    extends Visitor[H, OP] {

    protected def proxy[I, O](underlying: => G[I, O]): H[I, O]

    override def visitAnd[I, B, F[+_]](
      expr: And[I, B, F, OP],
    )(implicit
      logic: Conjunction[F, B, OP],
      opB: OP[F[B]],
    ): H[I, F[B]] = proxy(underlying.visitAnd(expr))

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

    override def visitConcatToHList[I, F[+_], WL <: HList : OP, UL <: HList](
      expr: ConcatToHList[I, F, WL, UL, OP],
    ): H[I, WL] = proxy(underlying.visitConcatToHList(expr))

    override def visitConst[O : OP](expr: Const[O, OP]): H[Any, O] = proxy(underlying.visitConst(expr))

    override def visitConvert[I, O : OP](expr: Convert[I, O, OP]): H[I, O] = proxy(underlying.visitConvert(expr))

    override def visitCustomFunction[I, O : OP](expr: CustomFunction[I, O, OP]): H[I, O] =
      proxy(underlying.visitCustomFunction(expr))

    override def visitExists[C[_] : Foldable, A, B : AsBoolean : OP](expr: Exists[C, A, B, OP]): H[C[A], B] =
      proxy(underlying.visitExists(expr))

    override def visitFilter[C[_] : FunctorFilter, A, B : AsBoolean : OP](
      expr: Filter[C, A, B, OP],
    )(implicit
      opO: OP[C[A]],
    ): H[C[A], C[A]] = proxy(underlying.visitFilter(expr))

    override def visitForAll[C[_] : Foldable, A, B : AsBoolean : OP](expr: ForAll[C, A, B, OP]): H[C[A], B] =
      proxy(underlying.visitForAll(expr))

    override def visitIdentity[I : OP](expr: Identity[I, OP]): H[I, I] = proxy(underlying.visitIdentity(expr))

    override def visitIsEqual[I, V, F[+_]](
      expr: IsEqual[I, V, F, OP],
    )(implicit
      eq: EqualComparable[F, V],
    ): H[I, F[Boolean]] = proxy(underlying.visitIsEqual(expr))

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

    override def visitOr[I, B, F[+_]](
      expr: Or[I, B, F, OP],
    )(implicit
      logic: Disjunction[F, B, OP],
      opO: OP[F[B]],
    ): H[I, F[B]] = proxy(underlying.visitOr(expr))

    override def visitSelect[I, O : OP](expr: Select[I, O, OP]): H[I, O] = proxy(underlying.visitSelect(expr))

    override def visitValuesOfType[T, O](expr: ValuesOfType[T, O, OP])(implicit opTs: OP[Seq[O]]): H[Any, Seq[O]] =
      proxy(underlying.visitValuesOfType(expr))

    override def visitWithinWindow[I, V, F[+_]](
      expr: WithinWindow[I, V, F, OP],
    )(implicit
      comparison: WindowComparable[F, OP],
      opV: OP[F[V]],
      opW: OP[F[Window[V]]],
      opB: OP[F[Boolean]],
    ): H[I, F[Boolean]] = proxy(underlying.visitWithinWindow(expr))

    override def visitZipToHList[I, F[+_] : Functor : Semigroupal, WL <: HList, UL <: HList](
      expr: ZipToHList[I, F, WL, UL, OP],
    )(implicit
      opO: OP[F[UL]],
    ): H[I, F[UL]] = proxy(underlying.visitZipToHList(expr))
  }

  // TODO: Use a NonEmptyList of expressions?
  /**
    * Evaluates the left and right expression nodes and uses `&&` to combine the results.
    *
    * @param leftExpr the left side of the `AND` operation
    * @param rightExpr the right side of the `AND` operation
    */
  final case class And[-I, +B, F[+_], OP[_]](
    leftExpr: Expr[I, F[B], OP],
    rightExpr: Expr[I, F[B], OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    logic: Conjunction[F, B, OP],
    opO: OP[F[B]],
  ) extends Expr[I, F[B], OP]("and") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[B]] = v.visitAnd(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): And[I, B, F, OP] =
      copy(debugging = debugging)
  }

  // TODO: Use a NonEmptyList of expressions?
  /**
    * Evaluates the left and right expression nodes and uses `||` to combine the results.
    *
    * @param leftExpr the left side of the `OR` operation
    * @param rightExpr the right side of the `OR` operation
    */
  final case class Or[-I, +B, F[+_], OP[_]](
    leftExpr: Expr[I, F[B], OP],
    rightExpr: Expr[I, F[B], OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    logic: Disjunction[F, B, OP],
    opB: OP[F[B]],
  ) extends Expr[I, F[B], OP]("or") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[B]] = v.visitOr(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Or[I, B, F, OP] =
      copy(debugging = debugging)
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
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
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
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
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
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    evLOisLI: LO <:< LI,
    evROisRI: RO <:< RI,
  ) extends Expr[I, O, OP]("combine") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitCombine(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Combine[I, LI, LO, RI, RO, O, OP] =
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
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
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
  final case class Exists[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    combineTrue: NonEmptyList[B] => B,
    combineFalse: List[B] => B,
    shortCircuit: Boolean,
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
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
  final case class ForAll[C[_] : Foldable, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    combineTrue: List[B] => B,
    combineFalse: NonEmptyList[B] => B,
    shortCircuit: Boolean,
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
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
  final case class Filter[C[_] : FunctorFilter, A, B : ExtractValue.AsBoolean : OP, OP[_]](
    conditionExpr: Expr[A, B, OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opO: OP[C[A]],
  ) extends Expr[C[A], C[A], OP]("filter") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], C[A]] = v.visitFilter(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Filter[C, A, B, OP] =
      copy(debugging = debugging)
  }

  /**
    * Passes the input to this expression as its output.
    *
    * You can think of this like the [[identity]] function.
    */
  final case class Identity[I : OP, OP[_]](private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging)
    extends Expr[I, I, OP]("identity") {
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
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opO: OP[C[B]],
  ) extends Expr[C[A], C[B], OP]("map") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[C[A], C[B]] = v.visitMapEvery(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): MapEvery[C, A, B, OP] =
      copy(debugging = debugging)
  }

  /**
    * Negates a given expression.
    *
    * [[Negation]] is tricky to define properly using constructivist logic. It is defined in this library
    * as the logical negation of the value with no change to the evidence.
    *
    * @param innerExpr the expression to negate
    */
  final case class Not[-I, +B, F[+_], OP[_]](
    innerExpr: Expr[I, F[B], OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    logic: Negation[F, B, OP],
    opB: OP[F[B]],
  ) extends Expr[I, F[B], OP]("not") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[B]] = v.visitNot(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Not[I, B, F, OP] =
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
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
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
    * @tparam F the wrapper type constructor for the context in which the values are compared
    */
  final case class IsEqual[-I, +V : OP, F[+_], OP[_]](
    leftExpr: Expr[I, F[V], OP],
    rightExpr: Expr[I, F[V], OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    eq: EqualComparable[F, V],
    opB: OP[F[Boolean]],
  ) extends Expr[I, F[Boolean], OP]("isEqual") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[Boolean]] = v.visitIsEqual(this)
    override def withDebugging(debugging: Debugging[Nothing, Nothing]): IsEqual[I, V, F, OP] =
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
    * @tparam F a container type, used to carry along metadata beyond just true / false.
    *           This can also be an effect type, but it must be covariant on its inner type.
    */
  final case class WithinWindow[-I, +V, F[+_], OP[_]](
    valueExpr: Expr[I, F[V], OP],
    windowExpr: Expr[I, F[Window[V]], OP],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    comparison: WindowComparable[F, OP],
    opV: OP[F[V]],
    opW: OP[F[Window[V]]],
    opB: OP[F[Boolean]],
  ) extends Expr[I, F[Boolean], OP]("withinWindow") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[Boolean]] = v.visitWithinWindow(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): WithinWindow[I, V, F, OP] =
      copy(debugging = debugging)
  }

  /**
    * Select or view a field of the input type using the specified [[VariantLens]].
    *
    * @param lens a lens from the input type to some selected field
    */
  final case class Select[-I, +O : OP, OP[_]](
    lens: VariantLens[I, O],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("select") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitSelect(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Select[I, O, OP] =
      copy(debugging = debugging)
  }

  final case class Convert[-I, +O : OP, OP[_]](
    converter: ExprConverter[I, O],
    private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, O, OP]("convert") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, O] = v.visitConvert(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): Convert[I, O, OP] =
      copy(debugging = debugging)
  }

  final case class ConcatToHList[-I, F[+_], +WL <: HList : OP, +UL <: HList, OP[_]](
    exprHList: NonEmptyExprHList[I, F, WL, UL, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  ) extends Expr[I, WL, OP]("concatToHList") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, WL] = v.visitConcatToHList(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): ConcatToHList[I, F, WL, UL, OP] =
      copy(debugging = debugging)
  }

  final case class ZipToHList[-I, F[+_] : Functor : Semigroupal, +WL <: HList, +UL <: HList, OP[_]](
    exprHList: NonEmptyExprHList[I, F, WL, UL, OP],
    override private[v1] val debugging: Debugging[Nothing, Nothing] = NoDebugging,
  )(implicit
    opO: OP[F[UL]],
  ) extends Expr[I, F[UL], OP]("zipToHList") {
    override def visit[G[-_, +_]](v: Visitor[G, OP]): G[I, F[UL]] = v.visitZipToHList(this)
    override private[v1] def withDebugging(debugging: Debugging[Nothing, Nothing]): ZipToHList[I, F, WL, UL, OP] =
      copy(debugging = debugging)
  }
}

![Vapor Logo](docs/logo.png)

<details>
<summary><strong>Table of Contents</strong></summary>

- [Vapors](#vapors)
- [Getting Started](#getting-started)
  * [Setup](#setup)
  * [Complex FactTypes](#complex-facttypes)
  * [Using a FactType Enumeration](#using-a-facttype-enumeration)
- [Terminology and Problem Statement](#terminology-and-problem-statement)
  * [Types of Expressions](#types-of-expressions)
  * [Expression Algebra](#expression-algebra)
  * [Fact Filter DSL](#fact-filter-dsl)
    + [DSL Operators](#dsl-operators)
      - [TerminalFactsExp Builders](#terminalfactsexp-builders)
      - [WhereBuilder Operators](#wherebuilder-operators)
  * [FactTypeSets](#facttypesets)
- [Implementation](#implementation)
  * [Interpreters](#interpreters)

<small><i><a href='http://ecotrust-canada.github.io/markdown-toc/'>Table of contents generated with markdown-toc</a></i></small>

</details>

[![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/jeffmay/vapors)](https://img.shields.io/github/v/tag/jeffmay/vapors)
[![codecov](https://codecov.io/gh/jeffmay/vapors/branch/v1/graph/badge.svg?token=M1WYH3T0XA)](https://codecov.io/gh/jeffmay/vapors)

# Vapors

The Vapors library provides an embedded-DSL for writing expressions that look similar to the Scala collections library
and can be interpreted to produce a function that computes the result given a set of source facts.

These expressions are descriptions of computations that can be optimized, serialized, and interpreted in various ways
using a tagless final recursive visitor (see [`Expr.Visitor`](core-v1/src/main/scala/algebra/Expr.scala)).

This library is built on top of Scala's `cats` library for typeclass definitions. See the
[Implementation](#implementation) section for more details.

You might want to also check out these [helpful slides on the library design](
https://docs.google.com/presentation/d/1eMGqw19Kba3Mw7ie36opsmgimFLrvTGfeMtPXkLMuQU/edit) for more details.
_(The slides are a little bit out of date, but the basic ideas are the same)_

# Getting Started

## Setup

1. **Add it to your `build.sbt`**

    ```sbt
    libraryDependencies += "com.rallyhealth" %% "vapors-v1" % "1.0.0-M2"
    ```

2. **Define your fact types.**

    Every fact type must have a unique name as well as a Scala type. You can define a `FactType` with any Scala type you
    want, so long as it defines an `Order`. This is so the facts can be pulled from the `FactTable` in a reasonable
    order.

    ```scala
    object FactTypes {
      val DateOfBirth = FactType[LocalDate]("DateOfBirth")
      val Role = FactType[Role]("Role")
    }

    sealed trait Role
    object Role {
      case object Admin extends Role
      case object User extends Role

      implicit val order: Order[Role] = Order.reverse[Role] {
        Order.by {
          case Admin => 1
          case User => 2
        }
      }
    }
    ```

    _Typically, if you have it, you'll order things by timestamp. Stay tuned for a data type that encapsulates
    `Timestamped` facts._

3. **Import the DSL.**

    You get different behavior and add support for different interpreters based on which DSL you use.

    If you just want to compute an expression quickly, with no justification wrapper, then you can use:
    ```
    import com.rallyhealth.vapors.v1.dsl.uncached._
    ```

    If you want your expression to produce a [`Justified`](core-v1/src/main/scala/data/Justified.scala) wrapped value,
    then you can use:
    ```
    import com.rallyhealth.vapors.v1.dsl.uncached.justified._
    ```

    Now when you call `.run()` or `.runWith()` on your expression, you can traverse the tree of justification for what
    operations produced that value.

4. **Craft your expressions.**

    Typically, you will start your expression using a value from the `FactTable`. You can use the `valuesOfType`
    operation to do so.

    Since every `FactType` can have multiple instances defined in the `FactTable`, they must also define an `Order`,
    so that the facts can come out of the `FactTable` in some natural ordering:

    ```scala
    import com.rallyhealth.vapors.v1.dsl.uncached._

    val isAdmin = {
      valuesOfType(FactTypes.Role).exists(_ === Role.Admin)
    }
    val isOver18 = {
      valuesOfType(FactTypes.DateOfBirth).exists { fact =>
        dateDiff(fact, today, ChronoUnit.YEARS.const) >= 18.const
      }
    }
    ```

5. **Run your expression with a set of facts.** 

    Assuming you have these facts:

    ```scala
    import FactTypes._

    val dob = FactTypes.DateOfBirth(LocalDate.of(1980, 1, 1))
    val adminRole = FactTypes.Role(Role.Admin)
    val userRole = FactTypes.Role(Role.User)

    val facts = FactTable(
      dob,
      adminRole,
      userRole
    )
    ```

    If you use the `justified` DSL, then you can get the list of facts (i.e. the `Evidence`) used to produce the output.  

    ```scala
    import com.rallyhealth.vapors.v1.dsl.uncached.justified._

    val isAdminResult = isAdmin.run(facts)
    assert(isAdminResult.value)
    assertEquals(isAdminResult.evidence, Evidence(adminRole))

    val combinedResult = and(isAdmin, isOver18).run(facts)
    assert(combinedResult.value)
    assertEquals(combinedResult.evidence, Evidence(adminRole, dob))
    ```

    If your expression requires anything other than `Any`, then you can only call `.runWith()` to provide the required
    input of the expected type.

    ```scala
    val isGt3 = ident[Int] > 3.const
    assert(isGt3.runWith(5))
    compileErrors("isGt3.run()")
    ```

## Complex FactTypes

The type of operators that are available for a type are dependent on the instances of cats typeclasses
that are available in implicit scope. So for example, if you want to be able to use `<`, `>=`, `===`, `=!=`, etc
then you need to define an `Order` instance for your type.

```scala
case class FeetAndInches(feet: Int, inches: Int)
object FeetAndInches {
  implicit val order: Order[FeetAndInches] = Order.fromLessThan { (l, r) =>
    l.feet < r.feet || (l.feet == r.feet && l.inches < r.inches)
  }
}
```

# Terminology and Problem Statement

In essence, every expression (`Expr[I, O, OP]`) can be optimized and interpreted to produce a function like:

```scala
(FactTable, I) => O
```

NOTE: If the `I` type is `Any`, then the framework will substitute
[`ExprState.Nothing`](core-v1/src/main/scala/data/ExprState.scala#L84) for the input and allow you to treat the
function like:

```scala
FactTable => O
```

The goal of this library is to provide a way to build serializable and introspectable definitions for facts. These
can also be thought of as "rules". Thus making this a rules engine library where the "rules" are built using a
strongly-typed, embedded domain-specific language (eDSL) using the Scala compiler.

You might wonder: Why not just use an existing rules engine DSL? Or why not just serialize native Scala code?

1. **Justification**

    The limited DSL allows computing a value alongside the chain of inferences that justifies that value from some set of source facts, config values, and / or constants.

    While many rules engines support serialization and ways of representing this capability, AFAICT none explicitly support it.

2. **Type-Safety**

    By using an embedded DSL, we take advantage of the Scala compiler to catch errors at compile-time and provide an IDE experience.

    While a goal of this project is to allow parsing expressions from files or strings, it should only be done so in a manner that does not violate strongly-typed schemas. In the meantime, we offer capabilities to operate on algebraic data types using `shapeless.HList` operations.

3. **Customization**

    By using embedding the construct within a fully-featured language, the library user is empowered to interpret and customize the DSL to suit their own needs with the full power of the Scala programming language with an interface that is designed for extension.

## Facts

### Data Types

#### Fact

Every `Fact` -- whether a _source fact_ or _derived fact_ -- is produced from a `FactType`.

#### FactType

A `FactType[T]` is constructed with both a `String` name and a Scala type parameter. The `FactType[T]` then acts like a
function that produces `TypedFact[T]` instances given values of type `T`.

```scala
val Age = FactType[Int]("age")
val sourceFact = Age(23)
```

#### TypedFact

All `TypedFact[_]` instances extend `Fact` which has a dependent type member called `Value` which is assigned to the
type parameter. This allows using these `Fact` instances inside invariant collections (like `Set[Fact]`). You could
also conceivably match on the `.factType` of a `Fact` to determine the `Value` type from the original `FactType[T]`.

#### TypedFactSet

In the case that you need to handle multiple facts with the same Scala type, but different names (thus different
`FactType[T]` definitions), you can use a `FactTypeSet[T]`. This can be useful if you are able to handle a common
supertype of multiple `FactType[_]` definitions in a common way. All operations that handle a single `FactType[T]`
can also handle a `FactTypeSet[T]`. Just be careful not to combine two different fact types with the same Scala type
that have different units of measurement. For example, `FactType[Double]("weight_kg")` and
`FactType[Double]("weight_lbs")` might have the same Scala type, `Double`, but you should be careful not to perform
calculations that are only valid if you are operating in the metric system.

#### FactTable

Every expression can be computed by providing a `FactTable`. This is a map of `FactType`s to instances of those
`TypedFact`s that can be retrieved at any point in the computation with `O(1)` performance.

### Terminology

#### Source Facts

When computing an expression, you will often want to feed the expression some starting set of **source facts**. These
facts are available in all computations using the `valuesOfType` operation, which can retrieve all instances of these
facts from the `FactTable` that was initially provided.

There is no distinction between **source facts** and **derived facts** beyond the understanding of when they were
defined. If they are defined as part of the process of evaluating an expression, then they are "derived", otherwise,
they must have been provided to the `FactTable` as "source" facts.

#### Derived Facts

You can use a `define` operation to create a **definition expression**, which can be provided to a `using` expression
to compute some values from the result of that definition expression having added all the values computed by the
expression into the `FactTable`.

```scala
import com.rallyhealth.vapors.v1.data.{FactType, FactTable}
import com.rallyhealth.vapors.v1.dsl.uncached._

import java.time.LocalDate
import java.time.temporal.ChronoUnit

val Age = FactType[Int]("age")
val DateOfBirth = FactType[LocalDate]("date_of_birth")
val defineAgeFromDob = define(Age).fromSeq {
  valuesOfType(DateOfBirth).map { dob =>
    dateDiff(today, dob, ChronoUnit.YEARS)
  }
}
val isOver18 = using(defineAgeFromDob).thenReturn {
  _.forall(_ >= 18.const)
}
// This will be true when run any day after 2007-12-31
assert(isOver18.run(FactTable(LocalDate.of(1990, 1, 1))))
```

## Expression Algebra

<table>
<tr>
  <th>Expr Signature<sup>*</sup></th>
  <th>Unwrapped DSL Type</th>
  <th>DSL Example</th>
  <th>Unwrapped Function<sup>**</sup></th>
  <th>Description</th>
</tr>
<tr>
  <td><code>Const[+O]</code></td>
  <td><code>Any ~:> O</code></td>
  <td><code>1.const</code></td>
  <td><code>_ => 1</code></td>
  <td>
    Ignores the input and just returns a constant value.
  </td>
</tr>
<tr>
  <td><code>Identity[I]</code></td>
  <td><code>I ~:> I</code></td>
  <td>
    <code>ident[Int] + 1</code><br/>
    an expression that adds 1 to its input.
  </td>
  <td><code>(_: Int) + 1</code></td>
  <td>
    The identity function. It just returns the input as the output. This has some applications for direct use, but is
    more commonly used to support the builder-style DSL.
    <br/>
    In an ideal world, these would be optimized out of the resulting expressions.
    <br/>
    <b>These are not often used directly.</b>
  </td>
</tr>
<tr>
  <td><code>AndThen[-II, +IO, -OI, +O]</code></td>
  <td><code>II ~:> OO</code></td>
  <td><code>
    ident[Seq[Seq[Int]]]
      .andThen(Expr.Flatten())
  </code></td>
  <td><code>
    (identity[Seq[Seq[Int]]] _)
      .andThen(_.flatten)
  </code></td>
  <td>
    Chain the output the first expression to the input of a second expression to produce the output of the second
    expression. This is the foundation for most of the DSL. Many operations only operate on a certain shape of input
    and rely on the `andThen` operation to chain the output of a previous expression into the input. 
  </td>
</tr>
<tr>
  <td><code>ValuesOfType[T, +O]</code></td>
  <td><code>Any ~:> Seq[O]</code></td>
  <td>
    <code>valuesOfType(Age)</code><br/>
    <br/>
    where <code>Age</code> is some <code>FactType[Int]</code>
  </td>
  <td><pre>(_, ft: FactTable) =>
  ft.getSortedSeq(FactTypeSet.one(Age))
    .map(_.value)</pre>
  </td>
  <td>
    Selects all the facts that have a <code>FactType</code> in the given <code>FactTypeSet</code>. The result of the
    expression will be an <code>Any ~:> Seq[O]</code> that can be filtered, mapped, etc to produce some typed
    computation result.<br/>
    NOTE: The only reason for the difference between `T` and `O` is that some DSLs apply a wrapper to every value in
    the DSL, so the output type might differ from the `FactType`
  </td>
</tr>
<tr>
  <td>
    <code>Definition[-I]</code>
    <p>/</p>
    <code>Define[-I, +C[_] : Foldable, T, OP]</code>
  </td>
  <td><code>I ~:> Seq[TypedFact[T]]</code></td>
  <td><code>define(Age).from(21.const)</code></td>
  <td><code>(age: Int, ft: FactTable) => ft.add(Age(age))</code></td>
  <td>
    Creates a <code>Definition[I, OP]</code> for a given <code>FactType[T]</code> with a given <code>Expr</code> node.
    These can be used in the <code>using</code> expression to guarantee that the facts generated for the given
    `FactType` are available for use in the `FactTable` in subsequent expressions within that scope.
  </td>
</tr>
<tr>
  <td><code>UsingDefinitions[-I, +O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><code>using(defineAge).thenReturn(_.exists(_ > 21))</code></td>
  <td><pre>(i: I, ft: FactTable) =>
  ft.addAll(defineAge(i))
    .getSortedSeq(Age)
    .map(_.value)
    .exists(_ > 21)</pre>
  </td>
  <td>
    Adds the given definitions to the <code>FactTable</code> and makes them available to the expression provided in the
    following block. 
  </td>
</tr>
<tr>
  <td><code>Combine[-I, -LI, +LO, -RI, +RO, +O]</code><sup>†</sup></td>
  <td><code>I ~:> O</code></td>
  <td><code>1.const + 2.const</code></td>
  <td><code>Add[Int, Int, Int].add(1, 2)</code></td>
  <td>
    Combine 2 expressions using a pre-defined binary operation. Every operation has a name and a function that combines
    the left-side input (<code>LI</code>) with the right-side input (<code>RI</code>) that will be provided the output
    of the two input expressions: left-side output (<code>LO</code>) and right-side output (<code>RO</code>).
    The result of the binary operation (<code>O</code>) is the final output.<br/>
    <code>Justified[Boolean]</code> defines all operations using <code>Justified.byInference</code> with the operation
    name and chains both the left and right side justification.
  </td>
</tr>
<tr>
  <td><code>And[-I, +B : AsBoolean, W[+_]]</code><sup>†</sup></td>
  <td><code>I ~:> Boolean</code></td>
  <td><code>and(true.const, false.const)</code></td>
  <td><code>Seq(true, false).forall(identity[Boolean])</code></td>
  <td>
    Combine expressions that return a value for which there is an <code>Conjunction</code> and combines the results
    using the given definition for conjunction.<br/>
    <code>Justified[Boolean]</code> defines <code>Conjunction</code> by returning the union of the evidence of all
    expressions that are truthy if any expressions are truthy, otherwise the union of all the evidence of all the
    given expressions that are falsy.
  </td>
</tr>
<tr>
  <td><code>Or[-I, +B : AsBoolean, W[+_]]</code><sup>†</sup></td>
  <td><code>I ~:> Boolean</code></td>
  <td><code>or(true.const, false.const)</code></td>
  <td><code>Seq(true, false).exists(identity[Boolean])</code></td>
  <td>
    Combine expressions that return a value for which there is an <code>Disjunction</code> and combines the results
    using the given definition for disjunction.<br/>
    <code>Justified[Boolean]</code> defines <code>Disjunction</code> by returning the union of the evidence of all
    expressions that are truthy if any expressions are truthy, otherwise the union of all the evidence of all the
    given expressions that are falsy.
  </td>
</tr>
<tr>
  <td><code>Not[-I, +B : AsBoolean, W[+_]]</code><sup>†</sup></td>
  <td><code>I ~:> Boolean</code></td>
  <td><code>not(true.const)</code></td>
  <td><code>!true</code></td>
  <td>
    Convert a falsy expression to a truthy expression (or vice versa) using a provided concept of
    <code>Negation</code><br/> The <code>Evidence</code> of a <code>Justified</code> value is not altered when the
    value is negated.
  </td>
</tr>
<tr>
  <td><code>Match[-I, +S, +B : AsBoolean, +O]</code></td>
  <td><code>I ~:> O</code></td>
  <td>
    <pre>ident[Person].matching(
  Case[Admin] ==> Role.Admin.const,
  Case[Employee].when {
    _.get(_.select(_.role)) >= Role.Editor.const
  } ==> ident[Employee].get(_.role),
)</pre>
  </td>
  <td><pre>(_: Person) match {
  case _: Admin => Some(Role.Admin)
  case e: Employee if e.role >= Role.Editor => Some(e.role)
  case _ => None
}</pre>
  </td>
  <td>
    Constructs an expression that matches on one of a series of branches that first matches on a subclass, then applies
    a filter expression, and finally, if the type is correct and the guard expression returns true, then it returns the
    output of the expression on the right-hand side of the <code>==></code> wrapped a <code>Some</code>. If no branch
    matches the input, then the expression returns <code>None</code>. See also
    <a href="/core-v1/src/main/scala/algebra/Expr.scala"><code>Expr.MatchCase[-I, S, +B, +O]</code></a>.
  </td>
</tr>
<tr>
  <td><code>When[-I, +B : AsBoolean, +O]</code></td>
  <td><code>I ~:> O</code></td>
  <td>
    <pre>when(true.const)
  .thenReturn(1.const)
  .elseReturn(0.const)</pre>
  </td>
  <td><code>if (true) 1 else 0</code></td>
  <td>
    Evaluates the first subexpression of the given sequence of condition branch expressions that evaluates to
    <code>true</code>. This is less powerful than <code>.matching(...)</code> in that it does not allow downcasting to
    a subclass before applying the condition expression. However, it does not return an <code>Option</code> and can be
    simpler and more clear in certain cases (including <code>Case[A].when()</code> conditions).
  </td>
</tr>
<tr>
  <td><code>Select[-I, A, B, +O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><code>ident[Map[String, String]].get(_.at("example")</code></td>
  <td><code>(_: Map[String, String]).get("example")</code></td>
  <td>
    Select a field from a product type, map, or sequence. Returns either an <code>Option</code> or strict value
    depending on the type of <code>Indexed</code> instance is available for input type and key type combination.
  </td>
</tr>
<tr>
  <td><code>Filter[C[_], A, +B : AsBoolean, D[_]]</code></td>
  <td><code>C[A] ~:> D[A]</code></td>
  <td><code>ident[NonEmptySeq[Int]].filter(_ > 1.const)</code></td>
  <td><code>(_: NonEmptySeq[Int]).filter(_ > 1)</code></td>
  <td>
    Keeps elements of the given collection for which the filter expression returns true and discards the others. 
  </td>
</tr>
<tr>
  <td><code>Match[I, S, B : AsBoolean, O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><pre>ident[RoleClass].matching(
  Case[Editor].when(_.get(_.select(_.articles)) > 0.const) ==>
    ident[Editor].get(_.select(_.permissions))
)</pre></td>
  <td><pre>(_: RoleClass) match {
  case e: Editor if e.articles > 0 => Some(e.permissions)
  case _ => None
}</pre></td>
  <td>
    Matches the given cases in the order provided. If any branch matches both the expected type and the guard
    expression, if any, then the result of the expression on the right-hand side of the <code>==></code> will be
    returned inside a <code>Some(_)</code>. If no <code>Case</code>s match, then the expression returns
    <code>None</code>.
  </td>
</tr>
<tr>
  <td><code>MapEvery[C[_] : Functor, A, B]</code></td>
  <td><code>C[A] ~:> C[B]</code></td>
  <td><code>ident[List[Int]].map(_ * 2.const)</code></td>
  <td><code>(_: List[Int]).map(_ * 2)</code></td>
  <td>
    For every value in the given <a href="https://typelevel.org/cats/api/cats/Functor.html"><code>Functor</code></a>
    apply the given expression to create a new collection of the same type with the elements produced by the given
    expression.
  </td>
</tr>
<tr>
  <td><code>Flatten[C[_] : FlatMap, A]</code></td>
  <td><code>C[C[A]] ~:> C[A]</code></td>
  <td><code>ident[List[List[Int]]].flatten</code></td>
  <td><code>(_: List[List[Int]]).flatten</code></td>
  <td>
    Uses the definition of <a href="https://typelevel.org/cats/api/cats/FlatMap.html"><code>FlatMap[F[_]]</code></a> to
    flatten the outer collection (<code>C[C[A]]</code>) into a new collection of all the elements in order
    (<code>C[A]</code>). This can be used to construct a <code>.flatMap()</code> operation.
  </td>
</tr>
<tr>
  <td><code>GetOrElse[I, O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><code>ident[Option[Int]].getOrElse(0.const)</code></td>
  <td><code>(_: Option[Int]).getOrElse(0)</code></td>
  <td>
    Calls the <code>Option.getOrElse</code> method on the result of an expression that returns an <code>Option[O]</code>
    and provides the evaluation of a default value expression to be computed if the original optional result is empty.
  </td>
</tr>
<tr>
  <td><code>Sorted[C[_], A]</code></td>
  <td><code>C[A] ~:> C[A]</code></td>
  <td><code>ident[List[Int]].sorted</code></td>
  <td><code>(_: List[Int]).sorted</code></td>
  <td>
    Sort the values using the given <code>ExprSorter</code> -- either a given natural <code>Order[R]</code> of the
    return type or the <code>Order</code> of a field selected by a given <code>NamedLens</code>).
  </td>
</tr>
<tr>
  <td><code>FoldLeft[-I, +C[_] : Foldable, A, O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><code>ident[List[Int]].foldLeft(0.const) { _ + _ }</code></td>
  <td><code>(_: List[Int]).foldLeft(0) { _ + _ }</code></td>
  <td>
    Folds the output of the given expressions into the result of some initial expression.
  </td>
</tr>
<tr>
  <td><code>Sequence[+C[+_] : Applicative : SemigroupK : Traverse, -I, +O]</code></td>
  <td><code>I ~:> C[O]</code></td>
  <td><code>wrapAll(NonEmptySeq.of(1.const, 2.const)])</code></td>
  <td><code>NonEmptySeq.of(_ => 1, _ => 2).map(_(()))</code></td>
  <td>
    Wraps the output of a sequence of given expressions into an expression that produces the same type of sequence from
    the results of every expression given the same input.
  </td>
</tr>
<tr>
  <td><code>ToHList[-I, +L &lt;: HList]</code></td>
  <td><code>I ~:> L</code></td>
  <td><code>wrap(1.const, "two".const).toHList</code></td>
  <td><code>((_ => 1)(()) :: (_ => "two")(()) :: HNil)</code></td>
  <td>
    Wraps the output of a heterogeneous <code>ExprHList</code> of given expressions into an <code>HList</code> of the
    return types.
  </td>
</tr>
<tr>
  <td><code>Convert[-I, +O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><code>(1 :: "two" :: HNil).const.as[(Int, String)]</code></td>
  <td><code>Generic[(Int, String)].from(1 :: "two" :: HNil)</code></td>
  <td>
    Converts an <code>HList</code> to any type that can be isomorphically converted using <code>shapeless.Generic</code> 
  </td>
</tr>
<tr>
  <td><code>ZipToShortestHList[-I, W[+_], +WL &lt;: HList, +UL &lt;: HList]</code></td>
  <td><code>I ~:> W[UL]</code></td>
  <td><code>wrap(seq(1.const, 2.const), seq("1".const)).zipToShortest</code></td>
  <td><code>(Seq(1, 2) :: Seq("1") :: HNil)</code></td>
  <td>
    Zips a heterogeneous list of expressions into a single <code>HList</code> of the results.
  </td>
</tr>
<tr>
  <td><code>Repeat[-I, +O]</code></td>
  <td><code>I ~:> IterableOnce[O]</code></td>
  <td><pre>wrap(
  Seq(1, 2, 3).const,
  repeatConstForever(0.const)
).zipToShortest</pre>
  </td>
  <td><pre>Seq(1, 2, 3)
  .zip(IterableOnce.continually(0))
  .map {
    case (l, r) } => l :: r :: HNil
  }</pre>
  </td>
  <td>
    Repeats the given expression (either memoizing the result or recomputing it) infinitely (or to some limit) to
    produce an expression of <code>IterableOnce[O]</code> that can be zipped with other sequences of known or unknown
    length. This is commonly used to thread a single value produced by a prior expression into the input of a
    collection operation (like <code>.map()</code>, <code>.forall</code>, etc). This helps to avoid the issue of lacking
    closures in the applicative expression language.
  </td>
</tr>
<tr>
  <td><code>SizeIs[-I, N : AsInt, B : AsBoolean]</code></td>
  <td><code>I ~:> B</code></td>
  <td><code>ident[Seq[Int]].sizeIs > 2.const</code></td>
  <td><code>(_: Seq[Int]).sizeIs > 2</code></td>
  <td>
    Returns <code>true</code> if the output of the expression has a size that meets a given <code>SizeComparison</code>.
  </td>
</tr>
<tr>
  <td><code>Slice[C[_] : Traverse, A, D[_]]</code></td>
  <td><code>C[A] ~:> D[A]</code></td>
  <td><code>ident[Seq[Int]].slice(1 <-> -1)</code></td>
  <td><code>(s: Seq[Int]) => s.slice(1, s.size - 1)</code></td>
  <td>
    Selects a range of values from the starting collection to produce an output collection. The <code>C[_]</code> and
    <code>D[_]</code> types are different because if you take a slice from a <code>NonEmptySeq</code>, you will get a
    regular <code>Seq</code> that can be empty. There are other collections that can differ when selected. You can even
    define your own. Check out the <code>CollectInto</code> type-level function.
  </td>
</tr>
<tr>
  <td><code>Exists[-C[_] : Foldable, A, B : AsBoolean]</code></td>
  <td><code>C[A] ~:> Boolean</code></td>
  <td><code>ident[Seq[Int]].exists(_ > 18.const)</code></td>
  <td><code>(_: Seq[Int]).exists(_ > 18)</code></td>
  <td>
    Returns <code>true</code> if there exists an element in the input collection for which the given predicate
    expression returns <code>true</code>, otherwise returns <code>false</code> (including if the collection is empty).
  </td>
</tr>
<tr>
  <td><code>ForAll[-C[_] : Foldable, A, B : AsBoolean]</code></td>
  <td><code>C[A] ~:> Boolean</code></td>
  <td><code>ident[Seq[Int]].forall(_ > 18.const)</code></td>
  <td><code>(_: Seq[Int]).forall(_ > 18)</code></td>
  <td>
    Returns <code>true</code> if every element in the input collection returns <code>true</code> from the given
    predicate expression (or the collection is empty), otherwise returns <code>false</code>.
  </td>
</tr>
<tr>
  <td><code>ContainsAny[-I, W[+_] : Extract, C[_] : Foldable, A, +B]</code><sup>†</sup></td>
  <td><code>I ~:> Boolean</code></td>
  <td><code>1.const in Set(1, 2, 3).const</code></td>
  <td><code>Set(1, 2, 3).contains(1)</code></td>
  <td>
    Returns <code>true</code> if the output of the expression is found in the given <code>Set[A]</code> of values,
    otherwise <code>false</code>.
  </td>
</tr>
<tr>
  <td><code>WithinWindow[-I, +V, W[+_]]</code><sup>†</sup></td>
  <td><code>I ~:> Boolean</code></td>
  <td><code>1.const <= 2.const</code></td>
  <td><code>1 <= 2</code></td>
  <td>
    Returns <code>true</code> if the output of the expression is contained by the given
    <a href="/core-v1/src/main/scala/data/Window.scala"><code>Window[V]</code></a>, otherwise <code>false</code>.<br/>
  </td>
</tr>
<tr>
  <td><code>IsEqual[-I, +V, W[+_]]</code><sup>†</sup></td>
  <td><code>I ~:> Boolean</code></td>
  <td><code>1.const === 2.const</code></td>
  <td><code>1 == 2</code></td>
  <td>
    Returns <code>true</code> if the output of the left expression is equal to the output of the right expression
    according to the provided definition of
    <a href="/core-v1/src/main/scala/algebra/EqualComparable.scala"><code>EqualComparable[W, V, OP]</code></a>.
  </td>
</tr>
<tr>
  <td><code>CustomFunction[-I, +O]</code></td>
  <td><code>I ~:> O</code></td>
  <td><code>Expr.CustomFunction("average", (xs: Seq[Int]) => xs.sum / xs.size)</code></td>
  <td><code>(xs: Seq[Int]) => xs.sum / xs.size</code></td>
  <td>
    Allows you do define a custom expression node that will evaluate the given function. Note that this will bypass any
    of the evidence tracking, introspect-ability, and other use cases for using this library in the first place, so you
    should only use it for simple functions that have well-defined behavior and name. This is mainly to allow calling
    out to library code to do things like decryption, datetime calculations, statistical calculations, etc.
  </td>
</tr>
</table>

<sup>*</sup>  Every `Expr` has 3 parameters, `-I` input, `+O` output, and `OP[_]` the output parameter.
              Since the `OP` type is repeated throughout every expression and has no real implication on the standard
              behavior of the language, I have removed it from the signatures. So, for example, if we have a subclass of
              `Expr` named `Op`, the signature would be shown as `Op[-I, +O]`, rather than `Op[-I, +O, OP[_]]`.

<sup>**</sup> Every `Expr[I, O, OP[_]]` built by an unwrapped DSL is interpreted as a function `(I, FactTable) => O`,
              however to simplify this column, any expression that does not depend on the `FactTable` will only use the
              input parameter, `I`.

<sup>†</sup>  These operations have a custom "wrapper" type `W[+_]`. This is not the ideal state for the expression
              algebra, as the wrapper type is an artifact of the DSL, and not the underlying logic of the language.
              Unfortunately, I was not able to figure out how to solve various issues that arose from using a single
              concrete type parameter (rather than a higher-kinded parameter and a concrete inner parameter).

## Expression Type Aliases

<table>
<tr>
  <th>Type</th>
  <th>Description</th>
  <th>Definition</th>
</tr>
<tr>
  <td><code>I ~:> O</code></td>
  <td>
    The super class of all expression nodes. You can call <code>.run()</code> on an <code>Any ~:> O</code> to get a
    <code>Result[O]</code>.
  </td>
  <td><code>Expr[I, O, OP]</code></td>
</tr>
<tr>
  <td><code>I =~:> O</code></td>
  <td>
    A function from an expression of <code>I ~:> I</code> to produce an expression <code>I ~:> O</code>.
  </td>
  <td><code>Expr[I, I, OP] => Expr[I, O, OP]</code></td>
</tr>
<tr>
  <td><code>AndThen[I, M, O]</code></td>
  <td>
    An alias for the <code>Expr.AndThen</code> expression node where the intermediate type is fixed.
  </td>
  <td><code>Expr.AndThen[I, M, M, O, OP]</code></td>
</tr>
<tr>
  <td><code>I >=< O</code></td>
  <td>
    An alias for the <code>Expr.WithinWindow</code> expression node where the wrapper type and OP type are fixed.
  </td>
  <td><code>Expr.WithinWindow[I, V, W, OP]</code></td>
</tr>
</table>

## Justification

## Custom DSLs / Interpreters

Note that the type `OP` is the "output parameter" that can be used when building an interpreter (i.e. `Expr.Visitor`)
to provide type-bounded context for the output of every expression node. It can also be used when defining mathematic
or comparison operations like addition, equality, etc. Lastly, it can be used when interpreting an `ExprResult`
(more details on this later).

In the simple `uncached` DSL, the result is computed directly without any wrapper. The interpreter produces a
stream-lined and efficient function because it can ignore any work required to wrap the results. You can define custom
DSLs and interpreters that produce different results, but they will likely be slower.

## Example

The following a more complete example of a set of facts and an evaluated query.

```scala
import cats.Order
import com.rallyhealth.vapors.v1.dsl.uncached.justified._

case class SemVer(major: Int, minor: Int, patch: Int)
object SemVer {
  implicit val order: Order[SemVer] = Order.fromLessThan { (l, r) =>
    l.major < r.major || l.major == r.major && l.minor < r.minor || l.major == r.major && l.minor == r.minor && l.patch < r.patch
  }
}

case class PredictionModel(
  modelName: String,
  modelVersion: SemVer,
  score: Double
)

object FactTypes {
  val DateOfBirth = FactType[LocalDate]("date_of_birth")
  val HeightFt = FactType[FeetAndInches]("height_ft")
  val WeightLbs = FactType[Int]("weight_lbs")
  val Prediction = FactType[PredictionModel]("prediction")
}

object JoeSchmoe {
  val height = FactTypes.HeightFt(FeetAndInches(5, 8))
  val weight = FactTypes.WeightLbs(260)
  val dateOfBirth = FactTypes.DateOfBirth(LocalDate.of(1980, 1, 1))
  val predictWeightloss = FactTypes.Prediction(PredictionModel("weightloss", SemVer(2, 0, 1), 0.85))
  val facts = FactTable(height, weight, dateOfBirth, predictWeightloss)
}

object Example {
  val query: Any ~:> Boolean = {
    or(
      and(
        valuesOfType(FactTypes.WeightLbs).exists(_ > 250.const),
        valuesOfType(FactTypes.HeightFt).exists(_ < FeetAndInches(6, 0).const)
      ),
      and(
        valuesOfType(FactTypes.WeightLbs).exists(_ > 300.const),
        valuesOfType(FactTypes.HeightFt).exists(_ >= FeetAndInches(6, 0).const)
      ),
      valuesOfType(FactTypes.Prediction).exists { prediction =>
        and(
          prediction.get(_.select(_.modelName)) === "weightloss".const,
          prediction.get(_.select(_.score)) > 0.8.const,
          prediction.get(_.select(_.modelVersion)) >= SemVer(2, 0, 0).const
        )
      }
    )
  }
  val rs = query.run(JoeSchmoe.facts)
  assert(rs.value)
  assert(rs.evidence == Evidence(JoeSchmoe.height, JoeSchmoe.weight, JoeSchmoe.predictWeightloss))
}
```

## Debugging

Since you cannot drop a breakpoint at a specific point in the construction of the expression (as you probably want
access to the value when it is running, rather than the mostly visible information available when the expression is
being constructed), we provide a mechanism to attach a debug function.

For example, you can call `.debug` on any `Expr` to provide a `ExprState[?, O] => Unit` function that will be evaluated
when the expression is run. By attaching this debug function, you also get place where you can drop a breakpoint.

Note: The input type of the `ExprState` provided to the debug function depends on the `Expr` subclass. This is the
primary reason why DSLs return the specific `Expr` subclass rather than their widened `I ~:> O` type. Likewise, the
specific `Expr` type is unaffected by attaching a debug function.

```scala
import com.rallyhealth.vapors.v1.dsl.uncached._

object Example {
  val query: Seq[String] ~:> Boolean = ident[Seq[String]].exists {
    _.sizeIs > 3
  }.debug { s =>
    val in = s.input
    val out = s.output
    println(s"INPUT: $in")
    println(s"OUTPUT: $out")
  }
}
```

# Implementation

## Core DSLs

| Import                        | Status | Interpreter      | Function Type                               | Function Description                                                               |
|:------------------------------|:-------|:-----------------|:--------------------------------------------|:-----------------------------------------------------------------------------------|
| `dsl.uncached`                | ✅     | `SimpleEngine`   | `Expr.State[Any, I] => O`                   | Produces the unwrapped value directly                                              |
| `dsl.uncached.justified`      | ☑️     | `SimpleEngine`   | `Expr.State[Any, I] => Justified[O]`        | Produces the `Justified` wrapped value directly                                    |
| `dsl.standard`                | ◽️     | `StandardEngine` | `Expr.State[Any, I] => ExprResult[I, O, OP]` | Produces the unwrapped value inside of an `ExprResult`, which can be reinterpreted |

✅ - Fully implemented and tested. Only minor changes to be expected.

☑️ - Fully implemented but with some inconsistencies. Major changes are plausible.

◽️ - Partially implemented. Major changes are likely.

## Interpreters

In order to convert from an `Expr` into something useful, we must interpret the expression.

The main interpreter is the engine provided by your imported DSL. If you are using an unwrapped DSL, then this is
probably the `SimpleEngine.Visitor` which converts an `Expr[I, O, OP]` into a function `(I, FactTable) => O`.

There is also a `Justified` DSL that uses the same `SimpleEngine`, but it returns a `Justified[O]` instead of just the 
`O`. This allows you to follow the chain of justification for any produced output.

Lastly, there is a `StandardEngine`<sup>†</sup> which interprets the `Expr` as a function that produces an `ExprResult`,
which is another recursive data structure that mirrors the expressions used to construct it. This mirror data structure
contains all the information of the original expression combined with the state and the captured output parameters at
the point where it was evaluated. This data structure can be re-interpreted with an `ExprResult.Visitor` to serialize
the evaluation, create some kind of visualization, etc. It is much slower to capture all of this data, so this DSL is
only recommended if you need any of these capabilities. This is a large part of the reason why the name will likely
change from "standard" to something more descrtiptive to its purpose.

<sup>†</sup> This engine requires redesign. The name and implementation are likely to change fairly dramatically.

### Custom Interpreters

You can define your own `Expr.Visitor` to produce something other than a function. For example, we can interpret an
`Expr` as a `Json` object by providing an interpreter that looks like:

```scala
import io.circe.Json
import io.circe.syntax._

object VisitExprAsJson {
  type G[-_, +_] = Json
}

class VisitExprAsJson[OP[a] <: HasEncoder[a]] extends Expr.Visitor[VisitExprAsJson.G, OP] {
  
  implicit def encodeOutput[O](implicit op: OP[O]): Encoder[O] = op.encoder
  
  override def visitConst[O : OP](expr: Expr.Const[O, OP]): Json = Json.obj(
    "$type" -> expr.name.asJson,
    "value" -> expr.value.asJson,
  )
  
  // ... implement all other visitX methods to produce Json
}
```

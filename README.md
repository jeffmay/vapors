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

# Vapors

The Vapors library provides an embedded-DSL for writing logical queries over a collection of facts.
These expressions can be optimized, evaluated, serialized, and interpreted in various ways in parallel.

This library is built on top of Scala's `cats-free` library. See the [Implementation](#implementation) section for more
details.

# Getting Started

## Setup

1.  **Add it to your `build.sbt`**

    ```sbt
    resolvers += Resolver.bintray("jeffmay", "maven")
    libraryDependencies += "com.rallyhealth" %% "vapors" % "0.1.0"
    ```

2.  **Define your fact types.**

    Every fact type must have a unique name as well as a Scala type. You can define a FactType with any Scala type you
    want, but if you want to be able to use comparison operators, you must define an `Ordering`. To check equality, you
    need to define `cats.Eq`.

    ```scala
    object FactTypes {
      val DateOfBirth = FactType[LocalDate]("DateOfBirth")
      val Role = FactType[Role]("Role")
    }
    
    sealed trait Role
    object Role {
      case object Admin extends Role
      case object User extends Role
      
      implicit val eq: Eq[Role] = Eq.fromUniversalEquals
      implicit val ordering: Ordering[Role] = Ordering.by {
        case Admin => 1
        case User => 2
      }.reverse
    }
    ```

3.  **Craft your expressions.**

    You must start with either a `TerminalFactsExp` or a logical operator (such as `and` / `or`). You will typically
    build a `TerminalFactsExp` by filtering to a specific type of fact and applying a filter condition on the instances.
    Since every `FactType` can have multiple instances, you have to decide how to filter the list with a logical rule:
    
    ```scala
    import cats.implicits._
    import com.rallyhealth.vapors.factfilter.dsl._
    
    val isAdmin = {
      withType(FactTypes.Role).whereAnyFactValue(_ === Role.Admin)
    }
    val isOver18 = {
      withType(FactTypes.DateOfBirth).whereAnyFactValue(_ <= LocalDate.now().minusYears(18))
    }
    ```
    
4.  **Feed your facts and expression into the evaluator.** 

    Assuming you have these facts:
    
    ```scala
    import FactTypes._
    
    val facts = Facts(
      DateOfBirth(LocalDate.of(1980, 1, 1)),
      Role("user"),
      Role("admin")
    )
    ```
    
    You can then evaluate the query to retrieve the facts that were used to prove that the query produces a sound result.  
    
    ```scala
    import com.rallyhealth.vapors.factfilter.dsl._
    
    val isAdminResult = evalWithFacts(facts) {
      isAdmin
    }
    assert(results.matchingFacts == List(Role("admin"))
    
    val combinedResult = evalWithFacts(facts) {
      isAdmin && isOver18
    }
    assert(results.matchingFacts == List(DateOfBirth(LocalDate.of(1980, 1, 1)), Role("admin"))
    ```
    
## Complex FactTypes

You can use any type you want in the `FactType`, but in order to use comparison operators like `>`, `>=`, `<`, `<=`, and `===`
you must define an implicit `Ordering` instance for that type.

```scala
case class FeetAndInches(feet: Int, inches: Int)
object FeetAndInches {
  implicit val ordering: Ordering[FeetAndInches] = Ordering.fromLessThan { (l, r) =>
    l.feet < r.feet || (l.feet == r.feet && l.inches < r.inches)
  }
}
```

## Using a FactType Enumeration 

You may want to put all of your `FactType` instances into an `Enumeration` for documentation and serialization purposes.

**TODO: Define a better interface to extend for enumerations** 

# Terminology and Problem Statement

In essence, every query can be optimized and evaluated to produce a function: 
```
Facts => ResultSet
```

## Types of Expressions

<table>
<tr>
  <th>Type</th>
  <th>Description</th>
  <th>Definition</th>
</tr>
<tr>
  <td><code>Exp[T, A]</code></td>
  <td>A generic expression.</td>
  <td>
    <code>FreeApplicative[ExpAlg[T, *, ...], A]</code>*
  </td>
</tr>
<tr>
  <td><code>TerminalFactsExp</code></td>
  <td>
    An expression that can refine an untyped set of <code>Facts</code>. Typically built using a <code>withType</code> selector.
  </td>
  <td>
    <code>Exp[Facts, ResultSet]</code>
  </td>
</tr>
<tr>
  <td><code>CondExp[T]</code></td>
  <td>
    A conditional expression that computes a <code>Boolean</code>.
  </td>
  <td>
    <code>Exp[T, Boolean]</code>
  </td>
</tr>
</table>

\* There may be other type parameters here, but the only types that typically matter are `T` and `A`. Currently, the
result of evaluating an expression is a function `T => A`.

## Expression Algebra

<table>
<tr>
  <th>ExpAlg</th>
  <th>Description</th>
</tr>
<tr>
  <td><code>And</code></td>
  <td>
    Combine conditional expressions to produce an empty result if any condition is false or the union of all facts
    required to prove the expressions to be true.
  </td>
</tr>
<tr>
  <td><code>Or</code></td>
  <td>
    Combine conditional expressions to produce an empty result if all conditions are false or the union of all facts
    that can be used to prove the expressions to be true.
  </td>
</tr>
<tr>
  <td><code>ForAll</code></td>
  <td>
    Perform a conditional expression on every element of an <code>IterableOnce</code> and return a false result if the
    condition is false for any value in the iterable otherwise returns a true result.
  </td>
</tr>
<tr>
  <td><code>Exists</code></td>
  <td>
    Perform a conditional expression on every element of an <code>IterableOnce</code> and return a true result if the
    condition is true for any value in the iterable otherwise returns a false result.
  </td>
</tr>
<tr>
  <td><code>Select</code></td>
  <td>
    Map a value into another type before performing an expression on the new type of input. Requires a total function.
  </td>
</tr>
<tr>
  <td><code>Collect</code></td>
  <td>
    Same as <code>Select</code>, but returns the empty set of facts if type refinement is impossible. Similar to a
    partial function.
  </td>
</tr>
<tr>
  <td><code>EqualTo</code></td>
  <td>
    Returns the values that equal the expected value.
  </td>
</tr>
<tr>
  <td><code>WithinWindow</code></td>
  <td>
    Returns the values the fall within the bounded or unbounded range.
  </td>
</tr>
</table>

## Fact Filter DSL

An embedded DSL that implements the algebra for `Facts` and returns a `ResultSet` that is either `FactsMatch` or
`NoFactsMatch`.

Every expressions starts with a `TerminalFactsExp` wrapping all sub-expressions. Typically, you will select the type
of fact you want to write your condition on using a `withType` builder. From there, you can choose to either apply the
operation to find *any* value or fact that matches or to make sure that *all* values or facts match the embedded
expression.

If you plan to use complex `FactType`s with custom data types, you probably want to also define typeclass instances to
support queries on these types. The main instances to define are `scala.Ordering` and `cats.Eq`.

The following a more complete example of a set of facts and an evaluated query.

```scala
import cats.implicits._
import com.rallyhealth.vapors.factfilter.dsl._

case class SemVer(major: Int, minor: Int, patch: Int)
object SemVer {
  implicit val ordering: Ordering[SemVer] = Ordering.fromLessThan { (l, r) =>
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
  val facts = Facts(height, weight, dateOfBirth, predictWeightloss)
}

val query: TerminalFactsExp = {
  or(
    and(
      withType(FactTypes.WeightLbs).whereAnyFactValue(_ > 250),
      withType(FactTypes.HeightFt).whereAnyFactValue(_ < FeetAndInches(6, 0)) 
    ),
    and(
      withType(FactTypes.WeightLbs).whereAnyFactValue(_ > 300),
      withType(FactTypes.HeightFt).whereAnyFactValue(_ >= FeetAndInches(6, 0)) 
    ),
    withType(FactTypes.Prediction).whereAnyFactValue { value =>
      and(
        value.at(_.select(_.modelName)) === "weightloss",
        value.at(_.select(_.score)) > 0.8,
        value.at(_.select(_.modelVersion)) >= SemVer(2, 0, 0)
      ) 
    }
  )
}
val rs = evalWithFacts(JoeSchmoe.facts)(query)
assert(rs.isTrue)
assert(rs.matchingFacts == Facts(JoeSchmoe.height, JoeSchmoe.weight, JoeSchmoe.predictWeightloss))
```

### DSL Operators

#### TerminalFactsExp Builders

These operators build `TerminalFactsExp`s that can be evaluated. All expressions will be wrapped by one of these
top-level expression nodes.

<table>
<tr>
  <th>Operator</th>
  <th>Sub-Expression Type</th>
  <th>Return Type</th>
  <th>Description</th>
</tr>
<tr>
  <td>
    <code>and</code> / <code>&&</code>
  </td>
  <td>
    <code>Exp[T, A]</code>
  </td>
  <td>
    <code>Exp[T, A]</code>
  </td>
  <td>
    Builds an expression that returns a falsey <code>A</code> when any sub-expression returns a falsey <code>A</code>
    result.
  </td>
</tr>
<tr>
  <td>
    <code>or</code> / <code>||</code>
  </td>
  <td>
    <code>Exp[T, A]</code>
  </td>
  <td>
    <code>Exp[T, A]</code>
  </td>
  <td>
    Builds an expression that returns a truthy <code>A</code> when any sub-expression returns a truthy <code>A</code>
    result.
  </td>
</tr>
<tr>
  <td>
    <code>withType[T]</code> / <code>withTypeIn[T]</code>
  </td>
  <td>
    N/A
  </td>
  <td>
    <code>WhereBuilder[T, ResultSet]</code>
  </td>
  <td>
    A <code>TerminalFactsExp</code> builder that filters facts to the provided type, collecting all facts that match
    the (eventually) provided <code>CondExp</code>
  </td>
</tr>
</table>

#### WhereBuilder Operators

**TODO: Finish these operators once we solidify the interface**

## FactTypeSets

Since `FactType`s have both a Scala type parameter and a `String` name, it is possible to handle multiple facts with
the same Scala type, but different names. Typically, you should only do this if these `FactType`s are all defined
with the same underlying meaning (although, they may differ in souce or quality). In this case, you can filter an
expression to a `FactTypeSet`.

# Implementation

In order to interpret expressions built by our embedded DSL, we wrap our `ExpAlg` algebraic data type (ADT)
in a [`FreeApplicative` data structure from the `cats-free` library](
https://typelevel.org/cats/datatypes/freeapplicative.html). This allows us to use operations such
as `.map` and `.foldMap` to produce new applicative contexts that can be evaluated efficiently.

## Interpreters

In order to go from `Exp[T, A]` to another type, such as a Json `Encoder[A]` or `Decoder[A]`, you just need
to define a *natural transformation* (also known as a `FunctionK[F[_], G[_]]`) from `ExpAlg[T, *] ~> G`.

Check out these [helpful slides on the library design](
https://docs.google.com/presentation/d/1eMGqw19Kba3Mw7ie36opsmgimFLrvTGfeMtPXkLMuQU/edit) for more details.

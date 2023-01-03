# Kerna Facts
## Vapors Role and DateOfBirth Facts, Justified!
* Paul A. Kennedy; paul.kennedy@{rally,optum}.com
* @ISODATE@

## Worked example of Vapors `Role` and `DateOfBirth` FactType

The Vapors README is a little schematic in its presentation.  In this
presentation we'll present some working examples.

We'll borrow from the vapors README a little bit by defining a couple of
`FactType`s.  We'll use a simple `LocalDate` to represent a date of birth.
We'll make a slightly more complicated type `Role` as the basis for our second
`FactType'.

Note that each needs to have an well defined `Order` implicit available.

```scala mdoc
import cats.Order
import com.rallyhealth.vapors.v1.data.FactType

import java.time.LocalDate

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

object FactTypes {
  import com.rallyhealth.vapors.v1.data.TimeOrder
  implicit val dobOrder: Order[LocalDate] = (new TimeOrder).orderLocalDate

  val DateOfBirth = FactType[LocalDate]("DateOfBirth")
  val Role = FactType[Role]("Role")
}
```

Next we'll define some `Fact`s.

A quick digression: we're going to use the `justified` flavour of the DSL because
it's a little different.  _N.B._ The imports need to be consistent across the
code because it affects the type signature (and is enforced by the compiler).

The `isAdmin` fact tests values from the `FactTable` and returns `true` if any
of them include the `Admin` role.

The `isOver18` fact tests if there are at least 18 years between `today` and
the `DateOfBirth` `LocatDate` values from the `FactTable`.


```scala mdoc
object FactsExample {
  import com.rallyhealth.vapors.v1.dsl.uncached.justified._
  import com.rallyhealth.vapors.v1.time.TemporalUnit

  // isAdmin: Any ~:> Boolean                for not justified
  // isAdmin: Any ~:> Justified[Boolean]     for justified
  val isAdmin = valuesOfType(FactTypes.Role).exists(_ === Role.Admin.const)

  // isOver18: Any ~:> Boolean                for not justified
  // isOver18: Any ~:> Justified[Boolean]     for justified
  val isOver18 = {
    valuesOfType(FactTypes.DateOfBirth).exists { fact =>
      dateDiff(fact, today, TemporalUnit.Years.const) >= 18L.const
    }
  }
}
```

Now, we can test the facts by setting up a `FactTable` and running the `Fact` on the `FactTable`.

```scala mdoc
object FactTests {

  import FactsExample._
  import com.rallyhealth.vapors.v1.dsl.uncached.justified._
  import com.rallyhealth.vapors.v1.data.FactTable
  import com.rallyhealth.vapors.v1.data.Evidence

  val dob = FactTypes.DateOfBirth(LocalDate.of(1980, 1, 1))
  val adminRole = FactTypes.Role(Role.Admin)
  val userRole = FactTypes.Role(Role.User)

  val facts = FactTable(
    dob,
    adminRole,
    userRole
  )

  val isAdminResult = isAdmin.run(facts)
  val isOver18Result = isOver18.run(facts)
  val combinedResult = and(isAdmin, isOver18).run(facts)

  def runTests(): Unit =  {
    // we need to extract the boolean for the justified DSL.  If we were
    // not using the justified DSL, the results would all be simple
    // booleans.
    assert(isAdminResult.value)
    assert(isOver18Result.value)
    assert(combinedResult.value)
    assert(combinedResult.evidence == Evidence(adminRole, dob))
    println("All assertions are satisfied.")
  }
}
```

And run the tests -- failures would be exceptions, of course, to the printed
output is an indication that the assertions passed.

```scala mdoc
FactTests.runTests()
```

## Colophon
This document was built with Vapors version **@VERSION@** and [mdoc](https://scalameta.org/mdoc/) on @ISODATE@.

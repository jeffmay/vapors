package com.rallyhealth.vapors.core.evaluator

import com.rallyhealth.vapors.core.Example.JoeSchmoe
import com.rallyhealth.vapors.core.data.{Fact, FactsMatch, NoFactsMatch}
import com.rallyhealth.vapors.core.dsl
import org.scalatest.wordspec.AnyWordSpec

class LogicalOperatorSpec extends AnyWordSpec {
  import dsl._

  private def True[T]: Fact[T] => Boolean = _ => true

  private def False[T]: Fact[T] => Boolean = _ => false

  "evaluator" when {

    "evaluating logical combinators" should {

      "return true for one true and one false in an or" in {
        val program = query {
          or(filter(True), filter(False))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }

      "return true for a complex or" in {
        val program = query {
          or(filter(False), filter(False), filter(True), filter(False))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }

      "return false for a long or" in {
        val program = query {
          or(filter(False), filter(False), filter(False), filter(False))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(NoFactsMatch()))
      }

      "return false for a true and false in an and" in {
        val program = query {
          and(filter(True), filter(False))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(NoFactsMatch()))
      }

      "return true for two trues in an and" in {
        val program = query {
          and(filter(True), filter(True))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }

      "return false for a complex and" in {
        val program = query {
          and(filter(True), filter(True), filter(False), filter(True))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(NoFactsMatch()))
      }

      "return true for a long and" in {
        val program = query {
          and(filter(True), filter(True), filter(True), filter(True))
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }

      "return true for nested true 'or's in an and" in {
        val program = query {
          and(
            or(filter(True), filter(True)),
            or(filter(True), filter(True)),
          )
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }

      "return false for a nested false 'or' before some true expressions" in {
        val program = query {
          and(
            or(
              filter(False),
              filter(False),
            ),
            filter(True),
            filter(True),
          )
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(NoFactsMatch()))
      }

      "return false for a nested false 'or' after some true expressions" in {
        val program = query {
          and(
            filter(True),
            or(
              filter(True),
              filter(False),
            ),
            or(
              filter(False),
              filter(False),
            ),
          )
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(NoFactsMatch()))
      }

      "return true for a nested false and in an 'or'" in {
        val program = query {
          or(
            filter(False),
            and(
              filter(False),
              filter(False),
            ),
            and(
              filter(True),
              filter(True),
            ),
            and(
              filter(True),
              filter(False),
            ),
          )
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }

      "return true for a complex structure" in {
        val program = query {
          or(
            filter(False),
            and(filter(False), filter(True)),
            or(filter(False), filter(False)),
            and(
              or(
                filter(True),
                filter(False),
              ),
              filter(False),
            ),
            or(
              and(
                filter(True),
                filter(False),
              ),
              filter(True),
            ),
          )
        }
        val result = evalQuery(JoeSchmoe.facts.toList)(program)
        assert(result.contains(FactsMatch(JoeSchmoe.facts)))
      }
    }
  }
}

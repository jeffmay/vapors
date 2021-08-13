package com.rallyhealth

package vapors.interpreter

import vapors.data.{Evidence, ExtractBoolean, FactSet, FactTable}
import vapors.dsl._
import vapors.example.JoeSchmoe
import vapors.logic._

import org.scalactic.source.Position
import org.scalatest.freespec.AnyFreeSpec

class LogicalExprSpec extends AnyFreeSpec {

  private type UExpr[R] = RootExpr[R, Unit]

  private type LogicOpBuilder[R] =
    (UExpr[R], UExpr[R], Seq[UExpr[R]]) => UExpr[R]

  private type UnaryLogicOpBuilder[R] = UExpr[R] => UExpr[R]

  private def validLogicalOperators[F[_], R](
    engine: VaporsEngine[F, Unit],
    andBuilder: LogicOpBuilder[R],
    orBuilder: LogicOpBuilder[R],
    notBuilder: UnaryLogicOpBuilder[R],
    trueBuilder: UExpr[R],
    falseBuilder: UExpr[R],
    facts: FactTable,
  )(
    assertTrue: Position => engine.Result[R] => Unit,
    assertFalse: Position => engine.Result[R] => Unit,
  ): Unit = {

    val T = trueBuilder
    val F = falseBuilder
    val not = notBuilder

    def and(
      one: UExpr[R],
      two: UExpr[R],
      tail: UExpr[R]*,
    ): UExpr[R] = andBuilder(one, two, tail)

    def or(
      one: UExpr[R],
      two: UExpr[R],
      tail: UExpr[R]*,
    ): UExpr[R] = orBuilder(one, two, tail)

    def evalOutput(expr: UExpr[R]): engine.Result[R] = engine.eval(expr, facts)

    def shouldBeTrue(output: engine.Result[R])(implicit pos: Position): Unit = {
      assertTrue(pos)(output)
    }

    def shouldBeFalse(output: engine.Result[R])(implicit pos: Position): Unit = {
      assertFalse(pos)(output)
    }

    "return 'false' for 'not true'" in {
      val q = {
        not(T)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for 'not false'" in {
      val q = {
        not(F)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for double negation of 'true'" in {
      val q = {
        not(not(T))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for double negation of 'false'" in {
      val q = {
        not(not(F))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for one 'true' and one 'false' in an 'or'" in {
      val q = {
        or(T, F)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for one 'false' and one 'not false' in an 'or'" in {
      val q = {
        or(F, not(F))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for one 'false' and one 'not true' in an 'or'" in {
      val q = {
        or(F, not(T))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for a complex 'or'" in {
      val q = {
        or(F, F, T, F)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for negating a complex 'or'" in {
      val q = {
        not(or(F, not(T), T, not(not(F))))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a long 'or'" in {
      val q = {
        or(F, F, F, F)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for negating a long 'or'" in {
      val q = {
        not(or(F, F, not(T), not(not(F))))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a 'true' and 'false' in an 'and'" in {
      val q = {
        and(T, F)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a 'true' and 'not true' in an 'and'" in {
      val q = {
        and(T, not(T))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for two trues in an and" in {
      val q = {
        and(T, T)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for two 'not false's in an 'and'" in {
      val q = {
        and(not(F), not(F))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a complex 'and'" in {
      val q = {
        and(T, T, F, T)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for the negation of a complex 'and'" in {
      val q = {
        not(and(T, not(F), F, not(not(T))))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for a long 'and'" in {
      val q = {
        and(T, T, T, T)
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for the negation of a long 'and'" in {
      val q = {
        not(and(T, not(F), not(not(T)), not(not(not(F)))))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return true for nested true 'or's in an 'and'" in {
      val q = {
        and(or(T, T), or(T, T))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for two nested 'not false or's in an 'and'" in {
      val q = {
        and(not(or(F, F)), not(or(F, F)))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a nested 'false or' before some 'true' expressions" in {
      val q = {
        and(or(F, F), T, T)
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for a nested 'not true or' before some 'true' expressions" in {
      val q = {
        and(not(or(F, T)), T, not(F))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a nested 'false or' after some 'true' expressions" in {
      val q = {
        and(T, or(T, F), or(F, F))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'false' for a nested 'not true or' after some 'true' expressions" in {
      val q = {
        and(T, not(F), or(not(T), T), or(F, not(T)))
      }
      shouldBeFalse(evalOutput(q))
    }

    "return 'true' for a nested 'false and' in an 'or'" in {
      val q = {
        or(F, and(F, F), and(T, T), and(T, F))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for the negation of a nested 'false and' in an 'or'" in {
      val q = {
        not(or(F, and(F, F), not(and(T, T)), and(T, not(F), not(T))))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'true' for a complex structure" in {
      val q = {
        or(F, and(F, T), or(F, F), and(or(T, F), F), or(and(T, F), T))
      }
      shouldBeTrue(evalOutput(q))
    }

    "return 'false' for a complex structure" in {
      val q = {
        not(
          or(
            F,
            not(T),
            and(F, T),
            or(F, F),
            not(and(T, T)),
            not(or(T, F)),
            not(not(F)),
            and(or(T, F), F, not(T)),
            or(F, not(T), and(T, F), not(and(T, not(F)))),
          ),
        )
      }
      shouldBeTrue(evalOutput(q))
    }
  }

  private abstract class DslLogicOpBuilder {

    def andBuilder[R : Conjunction : ExtractBoolean]: LogicOpBuilder[R]

    def orBuilder[R : Disjunction : ExtractBoolean]: LogicOpBuilder[R]

    def notBuilder[R : Negation]: UnaryLogicOpBuilder[R]
  }

  def validDslLogicalOperators[F[_]](
    engine: VaporsEngine[F, Unit],
    builder: DslLogicOpBuilder,
  )(implicit
    engineExtractParam: engine.ExtractParam,
  ): Unit = {

    behave like validLogicalOperators[F, Boolean](
      engine,
      builder.andBuilder,
      builder.orBuilder,
      builder.notBuilder,
      trueBuilder = const(true),
      falseBuilder = const(false),
      JoeSchmoe.factTable,
    )(
      assertTrue = { implicit pos => o =>
        val resultValue = engine.extract(o.value)
        assert(resultValue)
        for (evidence <- o.maybeEvidence) {
          // TODO: How to test evidence tracking / justification when we don't use the FactTable?
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      },
      assertFalse = { implicit pos => o =>
        val resultValue = engine.extract(o.value)
        assert(!resultValue)
        for (evidence <- o.maybeEvidence) {
          // TODO: How to test evidence tracking / justification when we don't use the FactTable?
          assertResult(Evidence.none) {
            engine.extract(evidence)
          }
        }
      },
    )
  }

  "and / or" - {

    val exprBuilder = new DslLogicOpBuilder {

      override def andBuilder[R : Conjunction : ExtractBoolean]: LogicOpBuilder[R] = { (one, two, tail) =>
        and(one, two, tail: _*)
      }

      override def orBuilder[R : Disjunction : ExtractBoolean]: LogicOpBuilder[R] = { (one, two, tail) =>
        or(one, two, tail: _*)
      }

      override def notBuilder[R : Negation]: UnaryLogicOpBuilder[R] = {
        not(_)
      }
    }

    "standard engine" - {
      validDslLogicalOperators(StandardVaporsEngine, exprBuilder)
    }

    "cats effect engine" - {
      import cats.effect.unsafe.implicits.global
      validDslLogicalOperators(CatsEffectSimpleVaporsEngine, exprBuilder)
    }
  }
}

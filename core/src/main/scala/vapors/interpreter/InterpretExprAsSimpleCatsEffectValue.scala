package com.rallyhealth

package vapors.interpreter

import vapors.algebra.{ConditionBranch, Expr}
import vapors.data._
import vapors.logic.{Conjunction, Disjunction, Negation}

import cats._
import cats.effect.IO
import shapeless.HList

import scala.collection.MapView

class InterpretExprAsSimpleCatsEffectValue[V](state: ExprInput[V]) extends Expr.Visitor[V, Unit, IO] {
  import vapors.math._

  import cats.implicits._

  override def visitAddOutputs[R : Addition](expr: Expr.AddOutputs[V, R, Unit]): IO[R] = {
    implicit val addSemigroup: Semigroup[IO[R]] = { (x, y) =>
      (x, y).mapN(Addition[R].add)
    }
    expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)
    }.reduce
  }

  override def visitAnd[R : Conjunction : ExtractBoolean](expr: Expr.And[V, R, Unit]): IO[R] = {
    implicit val conjunctionSemigroup: Semigroup[R] = Conjunction[R].conjunction
    expr.inputExprList
      .map { inputExpr =>
        inputExpr.visit(this)
      }
      .sequence
      .map { results =>
        results.reduce
      }
  }

  override def visitCollectSomeOutput[M[_] : Foldable, U, R : Monoid](
    expr: Expr.CollectFromOutput[V, M, U, R, Unit],
  ): IO[R] = {
    expr.inputExpr.visit(this).flatMap { inputValues =>
      val lazyCollectResults = inputValues.collectFirstSomeM { v =>
        val input = ExprInput(v, state.evidence, state.factTable)
        expr.collectExpr.visit(new InterpretExprAsSimpleCatsEffectValue(input))
      }
      lazyCollectResults.map(_.getOrElse(Monoid[R].empty))
    }
  }

  override def visitConcatOutput[M[_] : MonoidK, R](expr: Expr.ConcatOutput[V, M, R, Unit]): IO[M[R]] = {
    expr.inputExprList
      .traverse { inputExpr =>
        inputExpr.visit(new InterpretExprAsSimpleCatsEffectValue(state))
      }
      .map { outputs =>
        val M = MonoidK[M].algebra[R]
        outputs.reduceOption(M.combine).getOrElse(M.empty)
      }
  }

  override def visitConstOutput[R](expr: Expr.ConstOutput[V, R, Unit]): IO[R] = IO.pure(expr.value)

  override def visitCustomFunction[A, R](expr: Expr.CustomFunction[V, A, R, Unit]): IO[R] = {
    val inputResult = expr.inputExpr.visit(this)
    inputResult.map(expr.evaluate)
  }

  override def visitDefine[M[_] : Foldable, T](expr: Expr.Define[M, T, Unit]): IO[FactSet] = {
    val factTableInput = ExprInput(state.factTable, state.evidence, state.factTable)
    expr.definitionExpr.visit(new InterpretExprAsSimpleCatsEffectValue(factTableInput)).map { defnValues =>
      defnValues.foldMap { v =>
        FactSet(DerivedFact(expr.factType, v, state.evidence))
      }
    }
  }

  override def visitDivideOutputs[R : Division](expr: Expr.DivideOutputs[V, R, Unit]): IO[R] = {
    implicit val divisionSemigroup: Semigroup[R] = Division[R].quot
    expr.inputExprList
      .map { inputExpr =>
        inputExpr.visit(this)
      }
      .sequence
      .map { results =>
        results.reduce
      }
  }

  override def visitEmbed[R](expr: Expr.Embed[V, R, Unit]): IO[R] = {
    val input = ExprInput(state.factTable, state.evidence, state.factTable)
    expr.embeddedExpr.visit(new InterpretExprAsSimpleCatsEffectValue(input))
  }

  override def visitExistsInOutput[M[_] : Foldable, U](expr: Expr.ExistsInOutput[V, M, U, Unit]): IO[Boolean] = {
    expr.inputExpr.visit(this).flatMap { inputValues =>
      inputValues.existsM { v =>
        val condInput = ExprInput(v, state.evidence, state.factTable)
        expr.conditionExpr.visit(new InterpretExprAsSimpleCatsEffectValue(condInput))
      }
    }
  }

  override def visitFilterOutput[M[_] : TraverseFilter, R](expr: Expr.FilterOutput[V, M, R, Unit]): IO[M[R]] = {
    expr.inputExpr.visit(this).flatMap { inputValues =>
      inputValues.filterA { v =>
        val condInput = ExprInput(v, state.evidence, state.factTable)
        expr.condExpr.visit(new InterpretExprAsSimpleCatsEffectValue(condInput))
      }
    }
  }

  override def visitFlatMapOutput[M[_] : FlatMap : Traverse, U, X](
    expr: Expr.FlatMapOutput[V, M, U, X, Unit],
  ): IO[M[X]] = {
    expr.inputExpr.visit(this).flatMap { inputValues =>
      inputValues.flatTraverse { v =>
        val input = ExprInput(v, state.evidence, state.factTable)
        expr.flatMapExpr.visit(new InterpretExprAsSimpleCatsEffectValue(input))
      }
    }
  }

  override def visitGroupOutput[M[_] : Foldable, U : Order, K](
    expr: Expr.GroupOutput[V, M, U, K, Unit],
  ): IO[MapView[K, Seq[U]]] = {
    expr.inputExpr.visit(this).map { inputValues =>
      inputValues.toIterable.groupBy(expr.groupByLens.get).view.mapValues(_.to(LazyList))
    }
  }

  override def visitMapOutput[M[_] : Traverse, U, R](expr: Expr.MapOutput[V, M, U, R, Unit]): IO[M[R]] = {
    expr.inputExpr.visit(this).flatMap { inputValues =>
      inputValues.traverse { v =>
        val mapInput = ExprInput(v, state.evidence, state.factTable)
        expr.mapExpr.visit(new InterpretExprAsSimpleCatsEffectValue(mapInput))
      }
    }
  }

  override def visitMultiplyOutputs[R : Multiplication](expr: Expr.MultiplyOutputs[V, R, Unit]): IO[R] = {
    implicit val multiplySemigroup: Semigroup[IO[R]] = { (x, y) =>
      (x, y).mapN(Multiplication[R].multiply)
    }
    expr.inputExprList.map { inputExpr =>
      inputExpr.visit(this)
    }.reduce
  }

  override def visitNegativeOutput[R : Negative](expr: Expr.NegativeOutput[V, R, Unit]): IO[R] = {
    expr.inputExpr.visit(this).map(Negative[R].negative)
  }

  override def visitNot[R : Negation](expr: Expr.Not[V, R, Unit]): IO[R] = {
    expr.inputExpr.visit(this).map(Negation[R].negation)
  }

  override def visitOr[R : Disjunction : ExtractBoolean](expr: Expr.Or[V, R, Unit]): IO[R] = {
    implicit val disjunctionSemigroup: Semigroup[R] = Disjunction[R].disjunction
    expr.inputExprList
      .map { inputExpr =>
        inputExpr.visit(this)
      }
      .sequence
      .map { results =>
        results.reduce
      }
  }

  override def visitOutputIsEmpty[M[_] : Foldable, R](expr: Expr.OutputIsEmpty[V, M, R, Unit]): IO[Boolean] = {
    expr.inputExpr.visit(this).map { inputValues =>
      inputValues.isEmpty
    }
  }

  override def visitOutputWithinSet[R](expr: Expr.OutputWithinSet[V, R, Unit]): IO[Boolean] = {
    expr.inputExpr.visit(this).map { inputValue =>
      expr.accepted.contains(inputValue)
    }
  }

  override def visitOutputWithinWindow[R](expr: Expr.OutputWithinWindow[V, R, Unit]): IO[Boolean] = {
    (expr.inputExpr.visit(this), expr.windowExpr.visit(this)).mapN { (inputValue, window) =>
      window.contains(inputValue)
    }
  }

  override def visitFoldOutput[M[_] : Foldable, R : Monoid](expr: Expr.FoldOutput[V, M, R, Unit]): IO[R] = {
    expr.inputExpr.visit(this).map { inputValues =>
      inputValues.fold
    }
  }

  override def visitReturnInput(expr: Expr.ReturnInput[V, Unit]): IO[V] = IO.pure(state.value)

  override def visitSelectFromOutput[S, R](expr: Expr.SelectFromOutput[V, S, R, Unit]): IO[R] = {
    expr.inputExpr.visit(this).map { inputValue =>
      expr.lens.get(inputValue)
    }
  }

  override def visitSortOutput[M[_], R](expr: Expr.SortOutput[V, M, R, Unit]): IO[M[R]] = {
    expr.inputExpr.visit(this).map { inputValues =>
      expr.sorter(inputValues)
    }
  }

  override def visitSubtractOutputs[R : Subtraction](expr: Expr.SubtractOutputs[V, R, Unit]): IO[R] = {
    implicit val subtractSemigroup: Semigroup[R] = Subtraction[R].subtract
    expr.inputExprList
      .map { inputExpr =>
        inputExpr.visit(this)
      }
      .sequence
      .map { results =>
        results.reduce
      }
  }

  override def visitTakeFromOutput[M[_] : Traverse : TraverseFilter, R](
    expr: Expr.TakeFromOutput[V, M, R, Unit],
  ): IO[M[R]] = {
    expr.inputExpr.visit(this).map { inputValues =>
      val takeWindow: Window[Int] = expr.take match {
        case pos if pos > 0 => Window.between(0, pos)
        case 0 => Window.empty
        case neg if neg < 0 =>
          val totalSize = inputValues.size.toInt
          Window.between(totalSize + neg, totalSize)
      }
      val selectedValues = inputValues.zipWithIndex.collect {
        case (elem, idx) if takeWindow.contains(idx) => elem
      }
      selectedValues
    }
  }

  override def visitUsingDefinitions[R](expr: Expr.UsingDefinitions[V, R, Unit]): IO[R] = {
    val factTableInput = ExprInput(state.factTable, state.evidence, state.factTable)
    val runWithCurrentFactTable = new InterpretExprAsSimpleCatsEffectValue(factTableInput)
    expr.definitions
      .foldMapM { defn =>
        defn.visit(runWithCurrentFactTable)
      }
      .flatMap { newFacts =>
        val newFactTable = state.factTable.addAll(newFacts)
        val newFactTableInput = ExprInput(state.value, state.evidence, newFactTable)
        expr.subExpr.visit(new InterpretExprAsSimpleCatsEffectValue(newFactTableInput))
      }
  }

  override def visitWhen[R](expr: Expr.When[V, R, Unit]): IO[R] = {
    expr.conditionBranches
      .foldLeftM(None: Option[ConditionBranch[V, R, Unit]]) {
        case (matchedBranch @ Some(_), _) => IO.pure(matchedBranch)
        case (None, branch) =>
          branch.whenExpr.visit(this).map { conditionMet =>
            if (conditionMet) Some(branch)
            else None
          }
      }
      .flatMap { maybeBranch =>
        val branchExpr = maybeBranch.fold(expr.defaultExpr)(_.thenExpr)
        branchExpr.visit(this)
      }
  }

  override def visitWrapOutput[L, R](expr: Expr.WrapOutput[V, L, R, Unit]): IO[R] = {
    expr.inputExpr.visit(this).map { inputValue =>
      expr.converter(inputValue)
    }
  }

  override def visitWrapOutputHList[T <: HList, R](expr: Expr.WrapOutputHList[V, T, R, Unit]): IO[R] = {
    expr.inputExprHList.visitProduct(this).map { outputValue =>
      expr.converter(outputValue)
    }
  }

  override def visitWrapOutputSeq[R](expr: Expr.WrapOutputSeq[V, R, Unit]): IO[Seq[R]] = {
    expr.inputExprList.to(LazyList).traverse { inputExpr =>
      inputExpr.visit(this)
    }
  }

  override def visitWithFactsOfType[T, R](expr: Expr.WithFactsOfType[T, R, Unit]): IO[R] = {
    val matchingFacts = state.factTable.getSortedSeq(expr.factTypeSet)
    val input = ExprInput[Seq[TypedFact[T]]](matchingFacts, state.evidence, state.factTable)
    expr.subExpr.visit(new InterpretExprAsSimpleCatsEffectValue(input))
  }

  override def visitZipOutput[M[_] : Align : FunctorFilter, L <: HList, R](
    expr: Expr.ZipOutput[V, M, L, R, Unit],
  ): IO[M[R]] = {
    expr.inputExprHList.visitZippedToShortest(this).map { outputValues =>
      FunctorFilter[M].functor.map(outputValues)(expr.converter)
    }
  }
}

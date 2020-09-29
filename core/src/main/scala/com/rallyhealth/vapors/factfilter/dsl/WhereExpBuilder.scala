package com.rallyhealth.vapors.factfilter.dsl

import cats.data.NonEmptyList
import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.util.ReflectUtils.typeNameOf
import com.rallyhealth.vapors.factfilter.data._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

// TODO: Build this out more for code re-use?
trait WhereValuesExpBuilderOps[T, U, V] {

  def whereAnyValue(condExp: CondExp[V]): TerminalFactsExp

  def whereAllValues(condExp: CondExp[V]): TerminalFactsExp
}

final class WhereFactsExpBuilder[T, U <: T : ClassTag : TypeTag] private[dsl] (factTypeSet: FactTypeSet[U])
  extends WhereValuesExpBuilderOps[T, U, U] {

  // TODO: These terminal expression methods can be optimized to avoid an unnecessary select expression

  def whereAnyFact(condExp: CondExp[TypedFact[U]]): TerminalFactsExp = {
    new WhereValuesExpBuilder[T, U, TypedFact[U]](factTypeSet, NamedLens.id[TypedFact[U]]).whereAnyValue(condExp)
  }

  override def whereAnyValue(condExp: CondExp[U]): TerminalFactsExp = {
    new WhereValuesExpBuilder[T, U, U](factTypeSet, TypedFact.value[U]).whereAnyValue(condExp)
  }

  def whereAllFacts(condExp: CondExp[TypedFact[U]]): TerminalFactsExp = {
    new WhereValuesExpBuilder[T, U, TypedFact[U]](factTypeSet, NamedLens.id[TypedFact[U]]).whereAllValues(condExp)
  }

  override def whereAllValues(condExp: CondExp[U]): TerminalFactsExp = {
    new WhereValuesExpBuilder[T, U, U](factTypeSet, TypedFact.value[U]).whereAllValues(condExp)
  }

  def withValuesAt[V](lens: NamedLens.Id[U] => NamedLens[U, V]): WhereValuesExpBuilder[T, U, V] = {
    new WhereValuesExpBuilder(factTypeSet, TypedFact.value[U].andThen(lens(NamedLens.id[U])))
  }
}

final class WhereValuesExpBuilder[T >: U, U : ClassTag : TypeTag, V] private[dsl] (
  factTypeSet: FactTypeSet[U],
  factLens: NamedLens[TypedFact[U], V],
) extends WhereValuesExpBuilderOps[T, U, V] {

  private def selectFactValuesWhere(exp: ExpAlg[FactsOfType[U], TypedResultSet[U]]): TerminalFactsExp = {
    FreeApplicative.lift {
      ExpAlg.Collect[Facts, FactsOfType[U], ResultSet](
        s"Fact[${typeNameOf[U]}]",
        facts => NonEmptyList.fromList(facts.collect(factTypeSet.collector)),
        FreeApplicative.lift(exp).map(rs => rs: ResultSet),
        _ => NoFactsMatch(),
      )
    }
  }

  override def whereAllValues(condExp: CondExp[V]): TerminalFactsExp = {
    selectFactValuesWhere {
      ExpAlg.ForAll[FactsOfType[U], TypedFact[U], TypedResultSet[U]](
        _.toList,
        FreeApplicative.lift {
          ExpAlg.Select[TypedFact[U], V, Boolean](factLens, condExp)
        },
        FactsMatch(_),
        _ => NoFactsMatch(),
      )
    }
  }

  override def whereAnyValue(condExp: CondExp[V]): TerminalFactsExp = {
    selectFactValuesWhere {
      ExpAlg.Exists[FactsOfType[U], TypedFact[U], TypedResultSet[U]](
        _.toList,
        FreeApplicative.lift {
          ExpAlg.Select[TypedFact[U], V, Boolean](factLens, condExp)
        },
        facts => FactsMatch(facts),
        _ => NoFactsMatch(),
      )
    }
  }
}

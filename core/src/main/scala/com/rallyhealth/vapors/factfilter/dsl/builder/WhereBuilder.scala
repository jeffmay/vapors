package com.rallyhealth.vapors.factfilter.dsl.builder

import cats.free.FreeApplicative
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.data._
import com.rallyhealth.vapors.core.util.ReflectUtils.typeNameOf
import com.rallyhealth.vapors.factfilter.data._
import com.rallyhealth.vapors.factfilter.dsl.{CondExp, Exp, TerminalFactsExp}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

class WhereBuilder[T, U <: T : ClassTag : TypeTag] private[dsl] (factTypeSet: FactTypeSet[U]) {

  def where(buildExp: FactFilterBuilder[U] => Exp[List[TypedFact[U]], TypedResultSet[U]]): TerminalFactsExp = {
    val exp = buildExp(new FactFilterBuilder[U])
    collectFacts(exp)
  }

  @deprecated("Use where(_.value.exists(...)) instead.", "0.2.0")
  def whereAnyFact(buildExp: CondBuilder[TypedFact[U], TypedFact[U]] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereAny(TypedFact.lens[U], buildExp)
      }
    }
  }

  @deprecated("Use where(_.exists(...)) instead.", "0.2.0")
  def whereAnyFactValue(buildExp: CondBuilder[TypedFact[U], U] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereAny(TypedFact.value[U], buildExp)
      }
    }
  }

  private def whereAny[V](
    lens: NamedLens[TypedFact[U], V],
    buildExp: CondBuilder[TypedFact[U], V] => CondExp[TypedFact[U]],
  ): ExpAlg[List[TypedFact[U]], TypedResultSet[U]] = {
    ExpAlg.Exists[List[TypedFact[U]], TypedFact[U], TypedResultSet[U]](
      _.toList,
      buildExp(new CondBuilder(lens)),
      TypedResultSet.fromNel,
      _ => NoFactsMatch(),
    )
  }

  @deprecated("Use where(_.forall(...)) instead.", "0.2.0")
  def whereEveryFact(buildExp: CondBuilder[TypedFact[U], TypedFact[U]] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereEvery(TypedFact.lens[U], buildExp)
      }
    }
  }

  @deprecated("Use where(_.value.forall(...)) instead.", "0.2.0")
  def whereEveryFactValue(buildExp: CondBuilder[TypedFact[U], U] => CondExp[TypedFact[U]]): TerminalFactsExp = {
    collectFacts {
      FreeApplicative.lift {
        whereEvery(TypedFact.value[U], buildExp)
      }
    }
  }

  private def collectFacts(exp: Exp[List[TypedFact[U]], TypedResultSet[U]]): TerminalFactsExp = {
    FreeApplicative.lift {
      ExpAlg.Collect[Facts, List[TypedFact[U]], ResultSet](
        s"Fact[${typeNameOf[U]}]",
        facts => {
          val matching = facts.collect(factTypeSet.collector)
          Option.when(matching.nonEmpty)(matching)
        },
        exp.map(rs => rs: ResultSet),
        _ => NoFactsMatch(),
      )
    }
  }

  private def whereEvery[V](
    lens: NamedLens[TypedFact[U], V],
    buildExp: CondBuilder[TypedFact[U], V] => CondExp[TypedFact[U]],
  ): ExpAlg[List[TypedFact[U]], TypedResultSet[U]] = {
    ExpAlg.ForAll[List[TypedFact[U]], TypedFact[U], TypedResultSet[U]](
      _.toList,
      buildExp(new CondBuilder(lens)),
      _ => NoFactsMatch(),
      TypedResultSet(_),
    )
  }
}

package com.rallyhealth.vapors.v1

package data

import lens.Indexed

import cats.instances.order._
import cats.{Eq, Monoid}

import scala.collection.immutable.SortedMap

/**
  * The current state of all the facts in an expression.
  *
  * @note some expressions can update the fact table for sub-expressions.
  */
trait FactTable extends Any with IterableOnce[Fact] {
  protected type Self <: FactTable

  protected def build(factsByName: SortedMap[String, FactSet]): Self

  protected def factsByName: SortedMap[String, FactSet]

  def add(fact: Fact): FactTable = addAll(FactSet(fact))

  def addAll(facts: Iterable[Fact]): Self = {
    import cats.syntax.semigroup._
    val newFactTable = FactTable.fromSet(facts.toSet)
    build(this.factsByName |+| newFactTable.factsByName)
  }

  def getSortedSeq[T](factTypeSet: FactTypeSet[T]): IndexedSeq[TypedFact[T]] = {
    val sortedArray = getSet(factTypeSet).toArray.sortInPlace()
    sortedArray.toIndexedSeq
  }

  def getSet[T](factTypeSet: FactTypeSet[T]): TypedFactSet[T] = {
    val matchingFacts = for {
      factName <- factTypeSet.typeMap.toSortedMap.keys
      matchingFactsByName <- this.factsByName.get(factName)
    } yield matchingFactsByName.collect {
      case factTypeSet(matchingByType) => matchingByType
    }
    matchingFacts.reduceOption(_ | _).map(TypedFactSet.from).getOrElse(Set.empty)
  }

  final override def iterator: Iterator[Fact] = factsByName.valuesIterator.flatMap(_.iterator)

  // TODO: Better support for formatting here
  override def toString: String = if (factsByName.isEmpty) "FactTable.empty" else {
    val factMap = factsByName.iterator.map {
      case (factType, facts) =>
        s"\"$factType\": [${facts.mkString(", ")}]"
    }.mkString("{\n", ",\n", "\n}")
    s"FactTable($factMap)"
  }
}

object FactTable {

  final val empty = SimpleFactTable(SortedMap.empty)

  def apply(facts: FactOrFactSet*): FactTable = {
    val factSet = FactOrFactSet.flatten(facts)
    fromSet(factSet)
  }

  def fromSet(factSet: FactSet): FactTable = {
    if (factSet.isEmpty) empty
    else SimpleFactTable(SortedMap.from(factSet.groupBy(_.typeInfo.nameAndFullType)))
  }

  implicit object MonoidInstance extends Monoid[FactTable] {
    override def empty: FactTable = FactTable.empty
    override def combine(
      x: FactTable,
      y: FactTable,
    ): FactTable = {
      import cats.syntax.semigroup._
      SimpleFactTable(x.factsByName |+| y.factsByName)
    }
  }

  implicit val eq: Eq[FactTable] = Eq.fromUniversalEquals

  implicit def indexedByFactType[T]: Indexed[FactTable, FactType[T], TypedFactSet[T]] = {
    new Indexed[FactTable, FactType[T], TypedFactSet[T]] {
      override def get(container: FactTable)(key: FactType[T]): TypedFactSet[T] = {
        container.getSet(key)
      }
    }
  }

  implicit def indexedByFactTypeSet[T]: Indexed[FactTable, FactTypeSet[T], TypedFactSet[T]] = {
    new Indexed[FactTable, FactTypeSet[T], TypedFactSet[T]] {
      override def get(container: FactTable)(key: FactTypeSet[T]): TypedFactSet[T] = container.getSet(key)
    }
  }
}

/**
  * A simple value class wrapper around a Map of Strings to [[FactSet]]s.
  */
final case class SimpleFactTable(factsByName: SortedMap[String, FactSet]) extends AnyVal with FactTable {
  override protected type Self = SimpleFactTable
  override protected def build(factsByName: SortedMap[String, FactSet]): SimpleFactTable = SimpleFactTable(factsByName)
}

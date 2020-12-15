package com.rallyhealth.vapors.factfilter.data

import cats.Order
import cats.instances.order._

import scala.collection.immutable.SortedSet

final class FactOrFactSet private[FactOrFactSet] (val toSortedSet: SortedSet[Fact]) extends AnyVal

object FactOrFactSet {

  implicit def setOfOneFact(fact: Fact)(implicit order: Order[Fact]): FactOrFactSet = new FactOrFactSet(SortedSet(fact))

  implicit def iterableSetOfFacts(facts: Iterable[Fact])(implicit order: Order[Fact]): FactOrFactSet =
    new FactOrFactSet(SortedSet.from(facts))

  def flatten(factOrFactSets: Iterable[FactOrFactSet]): FactSet = {
    factOrFactSets.foldLeft(FactSet.empty)(_ | _.toSortedSet)
  }
}

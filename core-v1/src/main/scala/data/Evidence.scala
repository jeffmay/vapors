package com.rallyhealth.vapors.v1

package data

import cats.data.NonEmptySet
import cats.instances.order._

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/**
  * A set of [[Fact]]s with helpful operations.
  *
  * TODO: The mechanics of [[Justified]] results is still under construction. You should not rely on this type
  *       yet as it may be removed before v1.0.0.
  */
sealed trait Evidence {

  def factSet: SortedSet[Fact]

  def isEmpty: Boolean = factSet.isEmpty
  def nonEmpty: Boolean = factSet.nonEmpty

  @inline def ++(that: Evidence): Evidence = union(that)
  @inline def |(that: Evidence): Evidence = union(that)

  def ofType[T](factTypeSet: FactTypeSet[T]): Option[NonEmptySet[TypedFact[T]]] = {
    val matchingFacts = SortedSet.from(this.factSet.iterator.collect(factTypeSet.collector))
    NonEmptySet.fromSet(matchingFacts)
  }

  /**
    * Union of the two sets of results.
    */
  def union(that: Evidence): Evidence

  @inline def &(that: Evidence): Evidence = combineNonEmpty(that)

  /**
    * If either side contains empty evidence then the result contains no evidence.
    */
  def combineNonEmpty(that: Evidence): Evidence

  def derivedFromSources: Evidence = {
    @tailrec def loop(
      mixed: Iterable[Fact],
      source: SortedSet[Fact],
    ): SortedSet[Fact] = {
      if (mixed.isEmpty) source
      else {
        val (remainingEvidence, sourceFacts) = mixed.partitionMap {
          case DerivedFact(_, _, evidence) => Left(evidence.factSet)
          case source => Right(source)
        }
        loop(remainingEvidence.flatten, source ++ sourceFacts)
      }
    }
    val allSourceFacts = loop(this.factSet, SortedSet.empty)
    Evidence(allSourceFacts)
  }
}

object Evidence {

  def none: Evidence = NoEvidence

  def apply(factSets: FactOrFactSet*): Evidence = {
    val fullFactSet = factSets.foldLeft(Set.empty[Fact])(_ | _.toSet)
    NonEmptySet.fromSet(SortedSet.from(fullFactSet)).map(SomeEvidence).getOrElse(NoEvidence)
  }

}

case object NoEvidence extends Evidence {
  override def factSet: SortedSet[Fact] = SortedSet.empty
  override def union(that: Evidence): Evidence = that
  override def combineNonEmpty(that: Evidence): Evidence = that
}

case class SomeEvidence(facts: NonEmptySet[Fact]) extends Evidence {
  override def factSet: SortedSet[Fact] = facts.toSortedSet
  override def union(that: Evidence): SomeEvidence = that match {
    case NoEvidence => this
    case SomeEvidence(facts) => SomeEvidence(this.facts ++ facts)
  }
  override def combineNonEmpty(that: Evidence): Evidence = that match {
    case NoEvidence => NoEvidence
    case SomeEvidence(facts) => SomeEvidence(this.facts ++ facts)
  }
}

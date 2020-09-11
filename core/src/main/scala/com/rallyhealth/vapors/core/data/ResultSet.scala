package com.rallyhealth.vapors.core.data

import cats.SemigroupK
import cats.data.NonEmptyList

sealed trait ResultSet[+A] {

  def matchingFacts: List[Fact[A]]

  /**
    * Union of the two sets of results.
    */
  def union[B >: A](o: ResultSet[B]): ResultSet[B]

  def ++[B >: A](o: ResultSet[B]): ResultSet[B]
}

object ResultSet {

  def fromNel[I](nonEmptyFacts: NonEmptyList[Fact[I]]): FactsMatch[I] = FactsMatch(nonEmptyFacts)

  def fromList[I](facts: List[Fact[I]]): ResultSet[I] = NonEmptyList.fromList(facts) match {
    case Some(nonEmptyList) => FactsMatch(nonEmptyList)
    case None => NoFactsMatch()
  }

  // TODO: Should this be a MonoidK? It's feasible now, but maybe it isn't what we want
  implicit object Instances extends SemigroupK[ResultSet] {
    override def combineK[A](
      x: ResultSet[A],
      y: ResultSet[A],
    ): ResultSet[A] = x ++ y
  }
}

final case class FactsMatch[+I](matchingFactsNel: NonEmptyList[Fact[I]]) extends ResultSet[I] {

  override def matchingFacts: List[Fact[I]] = matchingFactsNel.toList

  private lazy val factSet: Set[Fact[Any]] = matchingFacts.toSet

  override def union[B >: I](o: ResultSet[B]): FactsMatch[B] = o match {
    case NoFactsMatch => this
    case FactsMatch(facts) =>
      val newFacts = facts.filterNot(factSet.contains(_))
      FactsMatch(matchingFactsNel ++ newFacts)
  }

  override def ++[B >: I](o: ResultSet[B]): FactsMatch[B] = union(o)
}

case object NoFactsMatch extends ResultSet[Nothing] {

  override def matchingFacts: List[Fact[Nothing]] = Nil

  override def union[B >: Nothing](o: ResultSet[B]): ResultSet[B] = o

  override def ++[B >: Nothing](o: ResultSet[B]): ResultSet[B] = o

  /**
    * For forwards / backwards compatibility with when this was / will become a case class.
    */
  def apply(): ResultSet[Nothing] = this
  def unapply(res: ResultSet[Any]): Boolean = res == this
}

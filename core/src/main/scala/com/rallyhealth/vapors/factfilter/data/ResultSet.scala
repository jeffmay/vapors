package com.rallyhealth.vapors.factfilter.data

import cats.data.NonEmptyList
import com.rallyhealth.vapors.core.logic.{Intersect, Union}

sealed abstract class ResultSet extends Equals {

  def matchingFacts: List[Fact]

  /**
    * Union of the two sets of results.
    */
  def union(o: ResultSet): ResultSet

  /**
    * Alias for [[union]].
    */
  @inline final def ++(o: ResultSet): ResultSet = this.union(o)

  // SAFE: ResultSet is sealed, so all subclasses are matched on by this parent type.
  def canEqual(other: Any): Boolean = other.isInstanceOf[ResultSet]

  override def equals(other: Any): Boolean = other match {
    case that: ResultSet =>
      that.canEqual(this) &&
        this.matchingFacts == that.matchingFacts
    case _ => false
  }

  override def hashCode(): Int = {
    val state = matchingFacts
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override final def toString: String = this match {
    case NoFactsMatch() => "NoFactsMatch()"
    case matched: FactsMatch => s"FactsMatch(${matched.matchingFactsNel})"
  }
}

object ResultSet {

  def apply(facts: List[Fact]): ResultSet = {
    NonEmptyList.fromList(facts).map(FactsMatch(_)).getOrElse(NoFactsMatch())
  }

  def fromNel(nonEmptyFacts: NonEmptyList[Fact]): FactsMatch = FactsMatch(nonEmptyFacts)

  implicit val unionAbstractResultSet: Union[ResultSet] = {
    _.reduceLeft[ResultSet] {
      case (NoFactsMatch(), b) => b // try the next expression if the first failed
      case (a, NoFactsMatch()) => a // use the previous expression if the next fails
      case (a, b) => a ++ b // union all the possible facts (for better quality calculations)
    }
  }

  implicit val intersectAbstractResultSet: Intersect[ResultSet] = {
    _.reduceLeft[ResultSet] {
      case (NoFactsMatch(), _) | (_, NoFactsMatch()) => NoFactsMatch() // skip the all expressions if the first failed
      case (acc, nextResult) => acc ++ nextResult // union all the required facts
    }
  }
}

sealed trait TypedResultSet[A] extends ResultSet {

  override def matchingFacts: List[TypedFact[A]]
}

object TypedResultSet {

  def apply[A](facts: List[TypedFact[A]]): TypedResultSet[A] = {
    NonEmptyList.fromList(facts).map(FactsMatch(_)).getOrElse(NoFactsMatch())
  }

  def fromNel[A](nonEmptyFacts: NonEmptyList[TypedFact[A]]): TypedFactsMatch[A] = FactsMatch(nonEmptyFacts)
}

// TODO: Should this be protected?
sealed class FactsMatch(val matchingFactsNel: NonEmptyList[Fact]) extends ResultSet {

  override def matchingFacts: List[Fact] = matchingFactsNel.toList

  private lazy val factSet: Set[Fact] = matchingFacts.toSet

  override def union(o: ResultSet): ResultSet = o match {
    case NoFactsMatch() => this
    case FactsMatch(facts) =>
      val newFacts = facts.collect(Function.unlift { fact =>
        if (factSet.contains(fact)) None
        else Some(fact)
      })
      new FactsMatch(matchingFactsNel ++ newFacts)
  }
}

object FactsMatch {

  def apply[T](matchingFactsNel: FactsOfType[T]): TypedFactsMatch[T] = {
    new TypedFactsMatch(matchingFactsNel)
  }

  def apply(matchingFactsNel: Facts): FactsMatch = {
    new FactsMatch(matchingFactsNel)
  }

  def unapply(matching: FactsMatch): Some[Facts] = {
    Some(matching.matchingFactsNel)
  }
}

final class TypedFactsMatch[A](override val matchingFactsNel: NonEmptyList[TypedFact[A]])
  extends FactsMatch(matchingFactsNel)
  with TypedResultSet[A] {

  override def matchingFacts: List[TypedFact[A]] = matchingFactsNel.toList
}

case object NoFactsMatch extends ResultSet with TypedResultSet[Nothing] {

  override def matchingFacts: List[TypedFact[Nothing]] = Nil

  override def union(o: ResultSet): ResultSet = o

  /**
    * For forwards compatibility for when this might become a case class.
    */
  def apply[T](): TypedResultSet[T] = this.asInstanceOf[TypedResultSet[T]]
  def unapply(res: ResultSet): Boolean = res == this
}

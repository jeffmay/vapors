package com.rallyhealth.vapors.core.data

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

final case class FactTypeSet[A] private (types: Map[String, FactType[A]]) {

  /**
    * Checks the [[FactType]] for equality with one of the types in this set.
    *
    * @return the [[Fact]] as the expected type.
    */
  def matchAs[B : ClassTag](fact: Fact[Any]): Option[Fact[B]] = matchAsPartial[B].lift(fact)

  /**
    *
    * @return a partial function for collecting facts of a specific type
    */
  def matchAsPartial[B : ClassTag]: PartialFunction[Fact[Any], Fact[B]] = {
    case fact @ Fact(tpe, _: B) if types.get(tpe.fullName).contains(tpe) =>
      // Justification: This checks equality of the FactType at runtime after safely casting the value
      fact.asInstanceOf[Fact[B]]
  }

  // TODO: Write unit tests for this to determine the expected utility of this method
  def subset[B](implicit tb: TypeTag[B]): Option[FactTypeSet[B]] = {
    val matchingFactTypes = types.toList.collect {
      // TODO: Should this use <:< instead of =:= ?
      case (_, factType) if factType.tt.tpe =:= tb.tpe =>
        // Justification: This checks equality of the FactType at runtime after safely casting the value
        factType.asInstanceOf[FactType[B]]
    }
    FactTypeSet.fromList(matchingFactTypes)
  }

  // TODO: Why is this Match implemented differently than matchAs?
  //       Either we should remove it or rewrite the above code to use it.
  final object Match {

    def apply(fact: Fact[_]): Option[Fact[A]] = {
      types.get(fact.typeInfo.name).flatMap(_.cast(fact))
    }

    def unapply(fact: Fact[_]): Option[Fact[A]] = apply(fact)
  }
}

object FactTypeSet {

  // TODO: Why have both empty and of methods? Shouldn't this be a NonEmptyMap?
  def empty[A]: FactTypeSet[A] = new FactTypeSet(Map())

  def of[A](
    one: FactType[A],
    others: FactType[A]*,
  ): FactTypeSet[A] = {
    new FactTypeSet(Map(one.fullName -> one) ++ others.map(a => (a.fullName, a)))
  }

  def fromSet[A](types: Set[FactType[A]]): Option[FactTypeSet[A]] = {
    fromList(types.toList)
  }

  def fromList[A](types: List[FactType[A]]): Option[FactTypeSet[A]] = {
    val typeMap = types.groupBy(_.fullName).collect {
      case (name, tpe :: Nil) => (name, tpe)
    }
    if (typeMap.size != types.size) None
    else Some(new FactTypeSet(typeMap))
  }
}

package com.rallyhealth.vapors.core.data

import cats.Functor
import cats.data.OneAnd

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.TypeTag

/**
  * Magnet type for [[Fact]]s and lifted values.
  */
sealed trait Data[+V] {
  def isValueOnly: Boolean
}

final case class Fact[+V](
  typeInfo: FactType[V],
  value: V
) extends Data[V] {
  override def isValueOnly: Boolean = false
}

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

  def subset[B](implicit tb: TypeTag[B]): Option[FactTypeSet[B]] = {
    val matchingFactTypes = types.toList.collect {
      case (_, factType) if factType.tt.tpe =:= tb.tpe =>
        // Justification: This checks equality of the FactType at runtime after safely casting the value
        factType.asInstanceOf[FactType[B]]
    }
    FactTypeSet.fromList(matchingFactTypes)
  }

  final object Match {

    def apply(fact: Fact[_]): Option[Fact[A]] = {
      types.get(fact.typeInfo.name).flatMap(_.cast(fact))
    }

    def unapply(fact: Fact[_]): Option[Fact[A]] = apply(fact)
  }
}

object FactTypeSet {

  def empty[A]: FactTypeSet[A] = new FactTypeSet(Map())

  def of[A](
    one: FactType[A],
    others: FactType[A]*
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

final case class Value[A](value: A) extends Data[A] {
  override def isValueOnly: Boolean = true
}

object Value {
  implicit object FunctorImpl extends Functor[Value] {
    override def map[A, B](fa: Value[A])(f: A => B): Value[B] = Value(f(fa.value))
  }
}

trait FactType[+T] {
  type Type <: T

  def name: String

  final lazy val fullName: String = s"$name: ${tt.tpe}"

  protected[vapors] implicit val ct: ClassTag[Type]
  protected[vapors] implicit val tt: TypeTag[Type]

  /**
    * Safely cast the given fact to this type.
    */
  def cast(fact: Fact[_]): Option[Fact[Type]] = {
    fact match {
      // Justification: This checks equality of the FactType after safely casting the fact value
      case f @ Fact(tpe, _: Type) if tpe == this => Some(f.asInstanceOf[Fact[Type]])
      case _ => None
    }
  }

  /**
    * Defines FactType equality as "both types have the same name and compiled type."
    */
  override final def equals(o: Any): Boolean = o match {
    case that: FactType[_] =>
      this.fullName == that.fullName && this.tt.tpe =:= that.tt.tpe
    case _ => false
  }

  override final def hashCode: Int = this.fullName.hashCode()
}

object FactType {

  def apply[T : ClassTag : TypeTag](name: String): FactType[T] = Simple(name)

  private final case class Simple[T : ClassTag : TypeTag](name: String) extends FactType[T] {
    override type Type = T
    override protected[vapors] val ct: ClassTag[T] = implicitly
    override protected[vapors] val tt: universe.TypeTag[T] = implicitly
  }
}

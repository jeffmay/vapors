package com.rallyhealth.vapors.core.data

import cats.Order

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait FactType[T] {

  def name: String

  final lazy val fullName: String = s"$name: ${tt.tpe}"

  protected[vapors] implicit val ct: ClassTag[T]
  protected[vapors] implicit val tt: TypeTag[T]

  /**
    * Validates the value is the correct type, but packages it as an [[Fact]] to avoid
    * issues with the compiler attempting to find the LUB of the [[TypedFact]] type parameter using
    * a wildcard.
    *
    * If you need the specific type of fact for some reason, you can use the standard [[TypedFact]]
    * factory method and supply this fact type as the first parameter.
    */
  def apply(value: T): Fact = TypedFact(this, value)

  /**
    * Safely cast the given fact to this type.
    */
  def cast(fact: TypedFact[_]): Option[TypedFact[T]] = {
    fact match {
      // Justification: This checks equality of the FactType after safely casting the fact value
      case f @ TypedFact(ft, _: T) if ft.tt.tpe <:< this.tt.tpe => Some(f.asInstanceOf[TypedFact[T]])
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
    override protected[vapors] val ct: ClassTag[T] = implicitly
    override protected[vapors] val tt: TypeTag[T] = implicitly
  }

  implicit class ToLens[T](private val ft: FactType[T]) extends AnyVal {
    def lens: NamedLens[TypedFact[T], T] = TypedFact.value[T]
  }

  implicit def orderingAny[T]: Ordering[FactType[T]] = Ordering.by(_.fullName)

  implicit def orderAny[T]: Order[FactType[T]] = Order.fromOrdering
}

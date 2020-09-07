package com.rallyhealth.vapors.core.data

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

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
    override protected[vapors] val tt: TypeTag[T] = implicitly
  }

  implicit class ToLens[T](private val ft: FactType[T]) extends AnyVal {
    def lens: NamedLens[Fact[T], T] = Fact.value[T]
  }
}

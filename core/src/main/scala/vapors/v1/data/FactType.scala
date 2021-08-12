package com.rallyhealth

package vapors.v1.data

import vapors.lens.{NamedLens, ValidDataPathKey}

import cats.Order
import izumi.reflect.Tag

import scala.annotation.switch
import scala.reflect.ClassTag

/**
  * Type information required to construct a [[Fact]].
  */
trait FactType[T] extends (T => TypedFact[T]) {

  /**
    * The unique name for this fact type.
    */
  def name: String

  /**
    * Definition for how to order fact values.
    */
  def order: Order[T]

  final def simpleTypeName: String = tt.tag.shortName

  final def fullTypeName: String = tt.tag.longName

  /**
    * The unique type name plus type information.
    *
    * Cached to avoid re-rendering on every call to .toString
    */
  final lazy val nameAndSimpleType: String = s"'$name' as $simpleTypeName"

  /**
    * The unique type name plus fully qualified type information.
    *
    * Cached to avoid re-rendering on every call to .hashCode
    */
  final lazy val nameAndFullType: String = s"'$name' as $fullTypeName"

  protected[vapors] implicit val ct: ClassTag[T]
  protected[vapors] implicit val tt: Tag[T]

  /**
    * Validates the value is the correct type, but packages it as an [[Fact]] to avoid
    * issues with the compiler attempting to find the LUB of the [[TypedFact]] type parameter using
    * a wildcard.
    *
    * If you need the specific type of fact for some reason, you can use the standard [[TypedFact]]
    * factory method and supply this fact type as the first parameter.
    */
  override def apply(value: T): TypedFact[T] = TypedFact(this, value)

  def unapply(value: Fact): Option[TypedFact[T]] = cast(value)

  /**
    * Safely cast the given fact to this type.
    */
  def cast(fact: Fact): Option[TypedFact[T]] = {
    fact match {
      // Justification: This checks validity of the FactType and safely casts the fact value using a class tag
      case f @ TypedFact(ft, _: T) if ft.tt.tag <:< this.tt.tag =>
        Some(f.asInstanceOf[TypedFact[T]])
      case f: Fact if f.typeInfo.tt.tag <:< this.tt.tag =>
        f.value match {
          case v: T => Some(TypedFact[T](f.typeInfo.asInstanceOf[FactType[T]], v))
          case _ => None
        }
      case _ => None
    }
  }

  /**
    * Defines FactType equality as "both types have the same name and compiled type."
    */
  override final def equals(o: Any): Boolean = o match {
    case that: FactType[_] =>
      this.nameAndFullType == that.nameAndFullType && this.tt.tag =:= that.tt.tag
    case _ => false
  }

  override final def hashCode: Int = this.nameAndFullType.hashCode()

  def productPrefix: String = "CustomFactType"

  override def toString: String = s"$productPrefix($nameAndSimpleType)"
}

object FactType {

  def apply[T : ClassTag : Tag : Order](name: String): FactType[T] = Simple(name)

  private final case class Simple[T : ClassTag : Tag : Order](name: String) extends FactType[T] {
    override val order: Order[T] = Order[T]
    override protected[vapors] val ct: ClassTag[T] = implicitly
    override protected[vapors] val tt: Tag[T] = Tag[T]
    override def productPrefix: String = "FactType"
    override def productElementName(n: Int): String = (n: @switch) match {
      case 0 => "name"
      case _ => super.productElementName(n)
    }
  }

  implicit class ToLens[T](private val ft: FactType[T]) extends AnyVal {
    def lens: NamedLens[TypedFact[T], T] = TypedFact.value[T]
  }

  implicit def orderingByName[T]: Ordering[FactType[T]] = Ordering.by(_.nameAndFullType)

  implicit def orderByName[T]: Order[FactType[T]] = Order.by(_.nameAndFullType)

  implicit def validDataPathKey[T]: ValidDataPathKey[FactType[T]] = _.nameAndFullType
}

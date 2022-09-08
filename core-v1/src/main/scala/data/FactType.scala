package com.rallyhealth.vapors.v1

package data

import lens.ValidDataPathKey

import cats.Order
import izumi.reflect.Tag

import scala.annotation.switch
import scala.reflect.ClassTag

/**
  * Type information required to construct a [[Fact]].
  */
trait AnyFactType {

  /**
    * The data type associated with facts of this type.
    */
  type Data

  /**
    * The unique name for this fact type.
    */
  def name: String

  /**
    * Definition for how to order fact values.
    */
  def order: Order[Data]

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

  protected[vapors] implicit val ct: ClassTag[Data]
  protected[vapors] implicit val tt: Tag[Data]

  /**
    * Convert from [[AnyFactType]] to a parameterized [[FactType]]. Typically, implemented as just `this`.
    */
  def parameterized: FactType[Data]

  /**
    * Validates the value is the correct type, but packages it as an [[Fact]] to avoid
    * issues with the compiler attempting to find the LUB of the [[TypedFact]] type parameter using
    * a wildcard.
    *
    * If you need the specific type of fact for some reason, you can use the standard [[TypedFact]]
    * factory method and supply this fact type as the first parameter.
    */
  def apply(value: Data): TypedFact[Data] = TypedFact(this.parameterized, value)

  def unapply(value: Fact): Option[TypedFact[Data]] = cast(value)

  /**
    * Safely cast the given fact to this type.
    */
  def cast(fact: Fact): Option[TypedFact[Data]] = {
    fact match {
      // Justification: This checks validity of the FactType and safely casts the fact value using a class tag
      case f@TypedFact(ft, _: Data) if ft.tt.tag <:< this.tt.tag =>
        Some(f.asInstanceOf[TypedFact[Data]])
      case f: Fact if f.typeInfo.tt.tag <:< this.tt.tag =>
        f.value match {
          case v: Data => Some(TypedFact[Data](f.typeInfo.asInstanceOf[FactType[Data]], v))
          case _ => None
        }
      case _ => None
    }
  }

  /**
    * Defines FactType equality as "both types have the same name and compiled type."
    */
  override final def equals(o: Any): Boolean = o match {
    case that: AnyFactType =>
      this.nameAndFullType == that.nameAndFullType && this.tt.tag =:= that.tt.tag
    case _ => false
  }

  override final def hashCode: Int = this.nameAndFullType.hashCode()

  def productPrefix: String = "CustomFactType"

  override def toString: String = s"$productPrefix($nameAndSimpleType)"
}

/**
  * Same as [[AnyFactType]], but implements [[Function1]] with the defined type.
  */
trait FactType[T] extends AnyFactType with (T => TypedFact[T]) {
  final override type Data = T
}

object FactType {

  def apply[T : ClassTag : Tag : Order](name: String): FactType[T] = Simple(name)

  private final case class Simple[T : ClassTag : Tag : Order](name: String) extends FactType[T] {
    override def parameterized: FactType[T] = this
    override val order: Order[T] = Order[T]
    override protected[vapors] val ct: ClassTag[T] = implicitly
    override protected[vapors] val tt: Tag[T] = Tag[T]
    override def productPrefix: String = "FactType"
    override def productElementName(n: Int): String = (n: @switch) match {
      case 0 => "name"
      case _ => super.productElementName(n)
    }
  }

  implicit def orderingByName[T]: Ordering[FactType[T]] = Ordering.by(_.nameAndFullType)

  implicit def orderByName[T]: Order[FactType[T]] = Order.by(_.nameAndFullType)

  implicit def validDataPathKey[T]: ValidDataPathKey[FactType[T]] = _.nameAndFullType
}

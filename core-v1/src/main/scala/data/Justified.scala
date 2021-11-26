package com.rallyhealth.vapors.v1

package data

import algebra.EqualComparable
import data.ExtractValue.AsBoolean
import logic.Logic
import math.{Add, Multiply, Subtract}

import cats.data.{NonEmptyList, NonEmptySet}
import cats.{Eq, Functor, Order}

import scala.annotation.nowarn

sealed trait Justified[+V] extends Product {

  def value: V

  def reason: String

  def visit[G[+_]](v: Justified.Visitor[G]): G[V]

  def configs: Seq[(String, Option[String])] // TODO: How should this handle duplicate keys?

  def evidence: Evidence

  def zipWith[Y, Z](
    that: Justified[Y],
    reason: String,
  )(
    fn: (V, Y) => Z,
  ): Justified[Z] =
    Justified.ByInference(
      reason,
      fn(this.value, that.value),
      NonEmptyList.of(this, that),
    )
}

object Justified {

  def apply[V](value: V): Justified[V] = ByConst(value)

  trait Visitor[G[+_]] {

    def visitConstant[V](justified: ByConst[V]): G[V]

    def visitConfig[V](justified: ByConfig[V]): G[V]

    def visitFact[V](justified: ByFact[V]): G[V]

    def visitInference[V](justified: ByInference[V]): G[V]
  }

  def byConst[V](value: V): Justified[V] = ByConst(value)

  final case class ByConst[V](value: V) extends Justified[V] {
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitConstant(this)
    override def evidence: NoEvidence.type = NoEvidence
    override val reason: String = s"[const: $value]"
    override def configs: Seq[(String, Option[String])] = Seq.empty
    override def productPrefix: String = "Justified.ByConst"
  }

  def byConfig[V](
    value: V,
    configKey: String,
    configDescription: Option[String] = None,
  ): Justified[V] = ByConfig(value, configKey, configDescription)

  final case class ByConfig[V](
    value: V,
    configKey: String,
    configDescription: Option[String] = None,
  ) extends Justified[V] {
    override def evidence: NoEvidence.type = NoEvidence
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitConfig(this)
    override val reason: String = s"[config: $configKey=$value]"
    override def configs: Seq[(String, Option[String])] = Seq(configKey -> configDescription)
    override def productPrefix: String = "Justified.ByConfig"
  }

  def byFact[V](fact: TypedFact[V]): Justified[V] = ByFact(fact)

  // TODO: Allow a lens for map operations? Many valuesOfType operations will be followed directly by a select
  //       operation. By using a lens here, we can easily chain map and select into a single operation to avoid
  //       more deeply nested Justified trees and save space / time when storing / interpreting them.
  // TODO: Alternatively, should there be a ByLens or BySelect justification that can chain multiple selects?
  final case class ByFact[V](fact: TypedFact[V]) extends Justified[V] {
    override val evidence: SomeEvidence = SomeEvidence(NonEmptySet.of(fact))
    override def value: V = fact.value
    override val reason: String = s"[fact: ${fact.typeInfo.name}=${fact.value}]"
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitFact(this)
    override def configs: Seq[(String, Option[String])] = Seq.empty
    override def productPrefix: String = "Justified.ByFact"
  }

  def byInference[V](
    reason: String,
    value: V,
    sources: NonEmptyList[Justified[Any]],
  ): Justified[V] = ByInference(reason, value, sources)

  final case class ByInference[V](
    reason: String,
    value: V,
    sources: NonEmptyList[Justified[Any]],
  ) extends Justified[V] {
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitInference(this)
    override lazy val configs: Seq[(String, Option[String])] =
      sources.map(_.configs).reduce // TODO: avoid clobbering duplicate keys
    override lazy val evidence: Evidence =
      sources.foldLeft(Evidence.none)(_ | _.evidence) // TODO: Is this even valid?
    override def productPrefix: String = "Justified.ByInference"
  }

  implicit def eq[V : Eq]: EqualComparable[Justified, V] = { (left, right) =>
    val isEqual = Eq[V].eqv(left.value, right.value)
    Justified.byInference("isEqual", isEqual, NonEmptyList.of(left, right))
  }

  implicit def orderingByValue[V : Ordering]: Ordering[Justified[V]] = Ordering.by(_.value)

  implicit def orderByValue[V : Order]: Order[Justified[V]] = Order.by(_.value)

  implicit def extractValue[V]: ExtractValue[Justified[V], V] = _.value

  implicit val functor: Functor[Justified] = new Functor[Justified] {
    override def map[A, B](fa: Justified[A])(f: A => B): Justified[B] = fa match {
      case Justified.ByConst(v) => Justified.byConst(f(v))
      case Justified.ByConfig(v, k, d) => Justified.byConfig(f(v), k, d)
      case _ =>
        val derived = f(fa.value)
        Justified.byInference("map", derived, NonEmptyList.of(fa))
    }
  }

  private val anyBool = new BooleanLogic[Boolean](identity)

  implicit def bool[OP[_]]: Logic[Justified, Boolean, OP] = anyBool.asInstanceOf[Logic[Justified, Boolean, OP]]

  final class BooleanLogic[B : AsBoolean](fromBoolean: Boolean => B) extends Logic[Justified, B, Any] {

    override def and(
      left: Justified[B],
      right: Justified[B],
    )(implicit
      opB: Any,
    ): Justified[B] = {
      val outcome = left.value && right.value
      Justified.byInference("and", fromBoolean(outcome), NonEmptyList.of(left, right))
    }

    override def or(
      left: Justified[B],
      right: Justified[B],
    )(implicit
      opB: Any,
    ): Justified[B] = {
      val outcome = left.value || right.value
      Justified.byInference("or", fromBoolean(outcome), NonEmptyList.of(left, right))
    }

    override def not(value: Justified[B])(implicit opB: Any): Justified[B] = {
      val outcome = !value.value
      Justified.byInference("not", fromBoolean(outcome), NonEmptyList.of(value))
    }
  }

  implicit def add[L, R, O](
    implicit
    addLR: Add.Aux[L, R, O],
  ): Add.Aux[Justified[L], Justified[R], Justified[O]] = {
    new Add[Justified[L], Justified[R]] {
      override type Out = Justified[O]
      def add(
        left: Justified[L],
        right: Justified[R],
      ): Justified[O] = {
        left.zipWith(right, "add")(addLR.add(_, _): @nowarn)
      }
    }
  }

  implicit def subtract[L, R, O](
    implicit
    subLR: Subtract.Aux[L, R, O],
  ): Subtract.Aux[Justified[L], Justified[R], Justified[O]] = {
    new Subtract[Justified[L], Justified[R]] {
      override type Out = Justified[O]
      def subtract(
        left: Justified[L],
        right: Justified[R],
      ): Justified[O] = {
        left.zipWith(right, "subtract")(subLR.subtract(_, _): @nowarn)
      }
    }
  }

  implicit def multiply[L, R, O](
    implicit
    multLR: Multiply.Aux[L, R, O],
  ): Multiply.Aux[Justified[L], Justified[R], Justified[O]] =
    new Multiply[Justified[L], Justified[R]] {
      override type Out = Justified[O]
      override def multiply(
        left: Justified[L],
        right: Justified[R],
      ): Justified[O] = left.zipWith(right, "multiply")(multLR.multiply(_, _): @nowarn)
    }
}

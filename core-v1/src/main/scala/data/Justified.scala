package com.rallyhealth.vapors.v1

package data

import algebra.EqualComparable
import dsl.{WrapConst, WrapFact, WrapQuantifier, WrapSelected}
import lens.{DataPath, VariantLens}
import logic.Logic
import math._

import cats.data.{NonEmptySeq, NonEmptySet}
import cats.implicits._
import cats.{Eq, Order, Traverse}

import scala.annotation.nowarn

/**
  * Represents a result that contains a tree of justified inputs and operations, as well as the value
  * for each operation along the way.
  *
  * Values can be justified by:
  * - Constants embedded into the expression itself
  * - Config values that are embedded into the expression but have a known configuration key / description
  * - Facts from the [[FactTable]] used to compute this output
  * - Inference by an operation defined by the [[dsl.JustifiedBuildExprDsl]] with references to all the
  *   justified values used as inputs to the operation.
  *
  * By following the chain of justification, one can determine if there is any factual evidence for a value,
  * or whether it is only supported by configs and constants (i.e. it is a default value and not one that is
  * tailored based on the provided facts).
  *
  * @tparam V the type of value that is justified by this container
  */
sealed trait Justified[+V] extends Product {

  /**
    * The output value from the expression.
    */
  def value: V

  /**
    * Keep the same justification, but adjust the lens.
    *
    * Use this with caution. It should only be used in situations where the value is an obvious isomorphic
    * transformation, such as a translation between a List and Vector, a value class and its value, a
    * known subtype / supertype, a temporary storage for a wrapper type that is later unwrapped, etc.
    */
  def withView[U >: V, A](buildLens: VariantLens.FromTo[U, A]): Justified[A] = {
    val thatLens = buildLens(VariantLens.id[U])
    val thatValue = thatLens.get(value)
    if (thatLens.path.isEmpty && thatValue == (this.value: Any)) this.asInstanceOf[Justified[A]]
    else
      this match {
        case Justified.BySelection(_, thisPath, thisSource) =>
          Justified.BySelection(thatValue, thisPath ++ thatLens.path, thisSource)
        case _ =>
          Justified.BySelection(thatValue, thatLens.path, this)
      }
  }

  /**
    * A short description for how the value is justified. Either a "const", "config", "fact", or the name of
    * an operation based on other justified values.
    */
  def reason: String

  /**
    * Collects all the configuration keys used to produce this [[value]]
    */
  def configs: Seq[(String, Option[String])] // TODO: How should this handle duplicate keys?

  /**
    * Collects all the [[Fact]]s used to produce this [[value]]
    */
  def evidence: Evidence

  /**
    * Visit the tree of justified values with a tagless-final visitor.
    */
  def visit[G[+_]](v: Justified.Visitor[G]): G[V]

  /**
    * Justify the result of an operation by inference given the other justified value, a reason, and a function
    * defining the operation.
    */
  def zipWith[Y, Z](
    that: Justified[Y],
    reason: String,
  )(
    fn: (V, Y) => Z,
  ): Justified[Z] =
    Justified.ByInference(
      reason,
      fn(this.value, that.value),
      NonEmptySeq.of(this, that),
    )
}

object Justified {

  trait Visitor[G[+_]] {

    def visitConstant[V](justified: ByConst[V]): G[V]

    def visitConfig[V](justified: ByConfig[V]): G[V]

    def visitFact[V](justified: ByFact[V]): G[V]

    def visitInference[V](justified: ByInference[V]): G[V]

    def visitSelection[V](justified: BySelection[V]): G[V]
  }

  def byConst[V](value: V): Justified[V] = ByConst(value)

  final case class ByConst[+V](value: V) extends Justified[V] {
    override def productPrefix: String = "Justified.ByConst"
    override val reason: String = s"[const: $value]"
    override def configs: Seq[(String, Option[String])] = Vector.empty
    override def evidence: NoEvidence.type = NoEvidence
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitConstant(this)
  }

  def byConfig[V](
    value: V,
    configKey: String,
    configDescription: Option[String] = None,
  ): Justified[V] = ByConfig(value, configKey, configDescription)

  final case class ByConfig[+V](
    value: V,
    configKey: String,
    configDescription: Option[String] = None,
  ) extends Justified[V] {
    override def productPrefix: String = "Justified.ByConfig"
    override val reason: String = s"[config: $configKey=$value]"
    override def configs: Seq[(String, Option[String])] = Vector(configKey -> configDescription)
    override def evidence: NoEvidence.type = NoEvidence
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitConfig(this)
  }

  def byFact[V](fact: TypedFact[V]): Justified[V] = ByFact(fact)

  final case class ByFact[V](fact: TypedFact[V]) extends Justified[V] {
    override def productPrefix: String = "Justified.ByFact"
    override val reason: String = s"[fact: ${fact.typeInfo.name}=${fact.value}]"
    override def value: V = fact.value
    override def configs: Seq[(String, Option[String])] = Vector.empty
    override val evidence: SomeEvidence = SomeEvidence(NonEmptySet.of(fact))
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitFact(this)
  }

  def bySelection[V](
    value: V,
    path: DataPath,
    source: Justified[Any],
  ): Justified[V] = BySelection(value, path, source)

  final case class BySelection[+V](
    value: V,
    path: DataPath,
    source: Justified[Any],
  ) extends Justified[V] {
    override def productPrefix: String = "Justified.BySelection"
    override val reason: String = s"[select: _${path.asString}]"
    override def configs: Seq[(String, Option[String])] = source.configs
    override def evidence: Evidence = source.evidence
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitSelection(this)
  }

  def byInference[V](
    reason: String,
    value: V,
    sources: NonEmptySeq[Justified[Any]],
  ): Justified[V] = ByInference(reason, value, sources)

  final case class ByInference[+V](
    reason: String,
    value: V,
    sources: NonEmptySeq[Justified[Any]],
  ) extends Justified[V] {
    override def productPrefix: String = "Justified.ByInference"
    override lazy val configs: Seq[(String, Option[String])] =
      sources.map(_.configs).reduce // TODO: avoid clobbering duplicate keys
    override lazy val evidence: Evidence =
      sources.foldLeft(Evidence.none)(_ | _.evidence) // TODO: Is this even valid?
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitInference(this)
  }

  def elements[C[_] : Traverse, A](collection: Justified[C[A]]): C[Justified[A]] = {
    collection.value.mapWithIndex { (a, idx) =>
      Justified.bySelection(a, DataPath.empty.atIndex(idx), collection)
    }
  }

  implicit def eq[V : Eq, OP[_]]: EqualComparable[Justified, V, OP] =
    new EqualComparable[Justified, V, OP] {
      override def isEqual(
        left: Justified[V],
        right: Justified[V],
      )(implicit
        opV: OP[Justified[V]],
        opO: OP[Justified[Boolean]],
      ): Justified[Boolean] = {
        val isEqual = Eq[V].eqv(left.value, right.value)
        Justified.byInference("isEqual", isEqual, NonEmptySeq(left, Vector(right)))
      }
    }

  implicit def orderingByValue[V : Ordering]: Ordering[Justified[V]] = Ordering.by(_.value)

  implicit def orderByValue[V : Order]: Order[Justified[V]] = Order.by(_.value)

  private final case object ExtractJustified extends Extract[Justified] {
    override def extract[A](fa: Justified[A]): A = fa.value
  }

  implicit def extract: Extract[Justified] = ExtractJustified

  implicit def extractValue[V]: ExtractValue[Justified[V], V] = _.value

  private final case object WrapConstJustified extends WrapConst[Justified, Any] {
    override def wrapConst[A](value: A)(implicit opA: Any): Justified[A] = Justified.byConst(value)
  }

  implicit def wrapConst[OP[_]]: WrapConst[Justified, OP] = WrapConstJustified.asInstanceOf[WrapConst[Justified, OP]]

  private final case object WrapFactJustified extends WrapFact[Justified, Any] {
    override def wrapFact[O](fact: TypedFact[O])(implicit opO: Any): Justified[O] = Justified.byFact(fact)
  }

  implicit def wrapFact[OP[_]]: WrapFact[Justified, OP] = WrapFactJustified.asInstanceOf[WrapFact[Justified, OP]]

  private final case object WrapSelectedJustified extends WrapSelected[Justified, Any] {
    override def wrapSelected[I, O](
      container: Justified[I],
      path: DataPath,
      element: O,
    )(implicit
      opA: Any,
      opB: Any,
    ): Justified[O] = {
      container.withView((_: Any) => VariantLens(path, (_: Any) => element))
    }
  }

  implicit def wrapSelected[OP[_]]: WrapSelected[Justified, OP] =
    WrapSelectedJustified.asInstanceOf[WrapSelected[Justified, OP]]

  private final object WrapQuantifierJustified extends WrapQuantifier[Justified, Any] {

    // TODO: Pull this from config somehow?
    override def shortCircuit: Boolean = false

    override def wrapFalseForAll(
      falseResults: NonEmptySeq[Justified[Boolean]],
    )(implicit
      opB: Any,
    ): Justified[Boolean] =
      Justified.byInference("forall", false, falseResults)

    override def wrapTrueForAll(trueResults: Seq[Justified[Boolean]])(implicit opB: Any): Justified[Boolean] = {
      NonEmptySeq
        .fromSeq(trueResults)
        .map { justified =>
          Justified.byInference("forall", true, justified)
        }
        .getOrElse {
          // forall in an empty collection is true
          // TODO: Should I put a reason instead of just a const?
          Justified.byConst(true)
        }
    }

    override def wrapFalseExists(falseResults: Seq[Justified[Boolean]])(implicit opB: Any): Justified[Boolean] = {
      NonEmptySeq
        .fromSeq(falseResults)
        .map { justified =>
          Justified.byInference("exists", false, justified)
        }
        .getOrElse {
          // exists in an empty collection is false
          // TODO: Should I put a reason instead of just a const?
          //       Maybe I should pass the original value in these functions?
          Justified.byConst(false)
        }
    }

    override def wrapTrueExists(trueResults: NonEmptySeq[Justified[Boolean]])(implicit opB: Any): Justified[Boolean] =
      Justified.byInference("exists", true, trueResults)
  }

  implicit def wrapQuantifier[OP[_]]: WrapQuantifier[Justified, OP] =
    WrapQuantifierJustified.asInstanceOf[WrapQuantifier[Justified, OP]]

  private val anyBool = new BooleanLogic[Boolean](identity)

  implicit def bool[OP[_]]: Logic[Justified, Boolean, OP] = anyBool.asInstanceOf[Logic[Justified, Boolean, OP]]

  final class BooleanLogic[B : ExtractValue.AsBoolean](fromBoolean: Boolean => B) extends Logic[Justified, B, Any] {

    override def and(
      left: Justified[B],
      right: Justified[B],
    )(implicit
      opB: Any,
    ): Justified[B] = {
      val outcome = left.value && right.value
      Justified.byInference("and", fromBoolean(outcome), NonEmptySeq(left, Vector(right)))
    }

    override def or(
      left: Justified[B],
      right: Justified[B],
    )(implicit
      opB: Any,
    ): Justified[B] = {
      val outcome = left.value || right.value
      Justified.byInference("or", fromBoolean(outcome), NonEmptySeq(left, Vector(right)))
    }

    override def not(value: Justified[B])(implicit opB: Any): Justified[B] = {
      val outcome = !value.value
      Justified.byInference("not", fromBoolean(outcome), NonEmptySeq.of(value))
    }
  }

  implicit def add[L, R, O](implicit add0: Add.Aux[L, R, O]): Add.Aux[Justified[L], Justified[R], Justified[O]] = {
    new Add[Justified[L], Justified[R]] {
      override type Out = Justified[O]
      def add(
        left: Justified[L],
        right: Justified[R],
      ): Justified[O] = {
        left.zipWith(right, "add")(add0.add(_, _): @nowarn)
      }
    }
  }

  implicit def subtract[L, R, O](
    implicit
    sub0: Subtract.Aux[L, R, O],
  ): Subtract.Aux[Justified[L], Justified[R], Justified[O]] = {
    new Subtract[Justified[L], Justified[R]] {
      override type Out = Justified[O]
      def subtract(
        left: Justified[L],
        right: Justified[R],
      ): Justified[O] = {
        left.zipWith(right, "subtract")(sub0.subtract(_, _): @nowarn)
      }
    }
  }

  implicit def multiply[L, R, O](
    implicit
    mult0: Multiply.Aux[L, R, O],
  ): Multiply.Aux[Justified[L], Justified[R], Justified[O]] =
    new Multiply[Justified[L], Justified[R]] {
      override type Out = Justified[O]
      override def multiply(
        left: Justified[L],
        right: Justified[R],
      ): Justified[O] = left.zipWith(right, "multiply")(mult0.multiply(_, _): @nowarn)
    }

  implicit def divide[N, D, O](
    implicit
    div0: Divide.Aux[N, D, O],
  ): Divide.Aux[Justified[N], Justified[D], Justified[O]] =
    new Divide[Justified[N], Justified[D]] {
      override type Out = Justified[O]
      override def divide(
        numerator: Justified[N],
        denominator: Justified[D],
      ): Justified[O] = numerator.zipWith(denominator, "divide")(div0.divide(_, _): @nowarn)
    }

  implicit def power[B, E, O](
    implicit
    pow0: Power.Aux[B, E, O],
  ): Power.Aux[Justified[B], Justified[E], Justified[O]] =
    new Power[Justified[B], Justified[E]] {
      override type Out = Justified[O]
      override def power(
        base: Justified[B],
        exponent: Justified[E],
      ): Justified[O] = base.zipWith(exponent, "power")(pow0.power(_, _): @nowarn)
    }
}

package com.rallyhealth.vapors.v1

package data

import math.Add

import cats.data.{NonEmptyList, NonEmptySet}

import scala.annotation.nowarn

sealed trait Justified[+V] extends Product {
  def value: V
//  def withValue[U](value: U): Justified[U]

  def visit[G[+_]](v: Justified.Visitor[G]): G[V]

  def configs: Seq[(String, Option[String])] // TODO: How should this handle duplicates?
  def evidence: Evidence

  def zipWith[Y, Z](that: Justified[Y])(fn: (V, Y) => Z): Justified[Z] =
    Justified.ByInference(
      fn(this.value, that.value),
      NonEmptyList.of(this, that),
    )

//  final def map[U](fn: V => U): Justified[U] = withValue(fn(value))

  override final def productPrefix: String = "Justified." + super.productPrefix
}

object Justified {

  def apply[V](value: V): Justified[V] = ByConst(value)

  trait Visitor[G[+_]] {

    def visitConstant[V](justified: ByConst[V]): G[V]

    def visitConfig[V](justified: ByConfig[V]): G[V]

    def visitFact[V](justified: ByFact[V]): G[V]

//    def visitEvidence[V](justified: ByEvidence[V]): G[V]

    def visitInference[V](justified: ByInference[V]): G[V]
  }

  def byConst[V](value: V): Justified[V] = ByConst(value)

  final case class ByConst[V](value: V) extends Justified[V] {
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitConstant(this)
    override def evidence: NoEvidence.type = NoEvidence
    override def configs: Seq[(String, Option[String])] = Seq.empty
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
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitConfig(this)
    override def evidence: NoEvidence.type = NoEvidence
    override def configs: Seq[(String, Option[String])] = Seq(configKey -> configDescription)
  }

//  final case class ByEvidence[V](
//    value: V,
//    override val evidence: SomeEvidence,
//  ) extends Justified[V] {
//    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitEvidence(this)
//    override def configs: Seq[(String, Option[String])] = Seq.empty
//  }

  def byFact[V](fact: TypedFact[V]): Justified[V] = ByFact(fact)

  // TODO: Allow a lens for map operations? Many valuesOfType operations will be followed directly by a select
  //       operation. By using a lens here, we can easily chain map and select into a single operation to avoid
  //       more deeply nested Justified trees and save space / time when storing / interpreting them.
  // TODO: Alternatively, should there be a ByLens or BySelect justification that can chain multiple selects?
  final case class ByFact[V](fact: TypedFact[V]) extends Justified[V] {
    override val evidence: SomeEvidence = SomeEvidence(NonEmptySet.of(fact))
    override def value: V = fact.value
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitFact(this)
    override def configs: Seq[(String, Option[String])] = Seq.empty
  }

  final case class ByInference[V](
    value: V,
    sources: NonEmptyList[Justified[Any]], // TODO: Should this be a NonEmptySet? How to sort it?
  ) extends Justified[V] {
    override def visit[G[+_]](v: Visitor[G]): G[V] = v.visitInference(this)
    override lazy val configs: Seq[(String, Option[String])] = sources.map(_.configs).reduce // TODO: should this be sorted?
    override lazy val evidence: Evidence = sources.foldLeft(Evidence.none)(_ | _.evidence) // TODO: Is this even valid?
  }

  implicit def add[L, R](
    implicit
    adder: Add[L, R],
  ): Add.Aux[Justified[L], Justified[R], Justified[adder.Out]] = {
    new Add[Justified[L], Justified[R]] {
      override type Out = Justified[adder.Out]
      def combine(
        left: Justified[L],
        right: Justified[R],
      ): Justified[adder.Out] = {
        left.zipWith(right)(adder.combine(_, _): @nowarn)
      }
    }
  }
}

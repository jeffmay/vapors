package com.rallyhealth.vapors.core.puml

import cats.Monoid

import scala.collection.BitSet

object Uml {

  type Label = String

  sealed trait Element[E] {

    def duplicates(that: E): Boolean
  }

  sealed trait Node extends Element[Node] {

    override def duplicates(that: Node): Boolean = this == that
  }

  // TODO: Add styling options
  final object Node

  final case object StartNode extends Node

  final case object StopNode extends Node

  sealed trait Labeled {
    self: Element[_] =>

    def label: Label
  }

  final case class LabeledNode(label: Label) extends Node with Labeled

  final case class ConditionNode(label: Label) extends Node with Labeled

  sealed trait Edge extends Element[Edge] {
    def startIdx: Int
    def endIdx: Int
    def startLabel: Option[Label]
    def endLabel: Option[Label]
    def direction: Option[Direction]

    def duplicates(that: Edge): Boolean = this.startIdx == that.startIdx && this.endIdx == that.endIdx
  }

  final case class Arrow(
    startIdx: Int,
    endIdx: Int,
    bidirectional: Boolean,
    length: Length,
    startLabel: Option[Label],
    endLabel: Option[Label],
    direction: Option[Direction],
  ) extends Edge {

    def flip: Arrow = copy(startLabel = endLabel, endLabel = startLabel)
  }

  final case class EdgeTarget(
    node: Node,
    bidirectional: Boolean = false,
    length: Length = Length.Medium,
    startLabel: Option[Label] = None,
    endLabel: Option[Label] = None,
    direction: Option[Direction] = None,
  )

  final object Graph {

    val empty = new Graph(Vector.empty, Vector.empty)

    def apply(): Graph = empty

    def apply(nodes: Seq[Node]): Graph = Graph(nodes, Nil)

    def apply(
      nodes: Seq[Node],
      edges: Seq[Edge],
    ): Graph = {
      new Graph(
        nodes = takeLatest(nodes),
        edges = takeLatest(edges),
      )
    }

    private def takeLatest[A](c: Seq[A]): IndexedSeq[A] = {
      val latestIndexes = for {
        indexes <- c.zipWithIndex.groupMap(_._1)(_._2).valuesIterator
      } yield indexes.last
      val latestIndexSet = BitSet.fromSpecific(latestIndexes)
      c.iterator.zipWithIndex.collect { case (n, idx) if latestIndexSet(idx) => n }.toVector
    }

    implicit object Instances extends Monoid[Graph] {
      override def empty: Graph = Graph.empty
      override def combine(
        x: Graph,
        y: Graph,
      ): Graph = x ++ y
    }

  }

  final class Graph private (
    val nodes: IndexedSeq[Node],
    val edges: IndexedSeq[Edge],
  ) {

    lazy val nodeSet: Set[Node] = nodes.toSet
    lazy val edgeSet: Set[Edge] = edges.toSet

    def addNode(node: Node): Graph = addNodes(Seq(node))

    def addNodes(nodes: Seq[Node]): Graph = new Graph(nodes = Graph.takeLatest(this.nodes ++ nodes), edges = edges)

    def addEdge(
      start: Node,
      end: EdgeTarget,
    ): Graph = {
      addEdges(start, List(end))
    }

    def addEdges(
      start: Node,
      targets: Seq[EdgeTarget],
    ): Graph = {
      if (targets.isEmpty) this
      else {
        val targetList = targets.toList
        val addNodes = start :: targetList.map(_.node)
        // TODO: This is a little ugly... any way to track the indexes of nodes that have yet to be added?
        var lastIdx = nodes.size - 1
        val startIdx = nodes.indexOf(start) match {
          case idx if idx < 0 =>
            lastIdx += 1
            lastIdx
          case idx => idx
        }
        val addEdges = for (t <- targetList) yield {
          val targetIdx = nodes.indexOf(t.node) match {
            case idx if idx < 0 =>
              lastIdx += 1
              lastIdx
            case idx => idx
          }
          Uml.Arrow(startIdx, targetIdx, t.bidirectional, t.length, t.startLabel, t.endLabel, t.direction)
        }
        Graph(nodes = nodes ++ addNodes, edges = edges ++ addEdges)
      }
    }

    def ++(o: Graph): Graph = Graph(edges = edges ++ o.edges, nodes = nodes ++ o.nodes)
  }
}

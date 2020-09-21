package com.rallyhealth.vapors.core.puml

import cats.data.State
import cats.{~>, Applicative, Show}
import com.rallyhealth.vapors.core.algebra._
import com.rallyhealth.vapors.core.dsl.Exp
import com.rallyhealth.vapors.core.puml.Uml.{EdgeTarget, Labeled, StartNode, StopNode}

class PlantUml {

  def createGraph(expression: Exp[_, _]): Uml.Graph = {
    def create[A, B](exp: Exp[A, B]): PlantUml.GraphState[B] = {
      exp.foldMap(PlantUml.GraphConverter())(PlantUml.GraphStateInstances)
    }
    create(expression).runEmptyS.value
  }

  def serialize(graph: Uml.Graph): String = {
    val sb = new StringBuilder

    def writeNode(node: Uml.Node): Unit = node match {
      case labeled: Labeled => sb.append('"').append(labeled.label).append('"')
      case StartNode => sb.append("start")
      case StopNode => sb.append("stop")
      case _ =>
    }
    for (n <- graph.nodes) {
      writeNode(n)
      sb.append('\n')
    }
    sb.append('\n')
    for (e <- graph.edges) {
      val startNode = graph.nodes(e.startIdx)
      val endNode = graph.nodes(e.endIdx)
      writeNode(startNode)
      sb.append(" --> ")
      writeNode(endNode)
      sb.append('\n')
    }
    sb.result().trim
  }

}

object PlantUml extends PlantUml {

  private type NewNodes = List[Uml.Node]
  private type GraphState[A] = State[Uml.Graph, NewNodes]

  private implicit final object GraphStateInstances extends Applicative[GraphState] {
    override def pure[A](x: A): GraphState[A] = State.pure(Nil)
    override def ap[A, B](ff: GraphState[A => B])(fa: GraphState[A]): GraphState[B] = {
      for {
        g1 <- ff.get
        g2 <- fa.get
        _ <- State.set(g1 ++ g2)
      } yield Nil
    }
  }

  private object GraphConverter extends (ExpAlg[Any, *] ~> GraphState) {

    private def loopSubExpressionAndAddEdges[A](
      init: Uml.Graph,
      sub: Exp[Any, A],
      newNode: Uml.Node,
    ): (Uml.Graph, NewNodes) = {
      val (subGraph, startNodes) = sub.foldMap(this).run(init).value
      (subGraph.addEdges(newNode, startNodes.map(EdgeTarget(_))), newNode :: Nil)
    }

    override def apply[A](fa: ExpAlg[Any, A]): GraphState[A] = {
      import cats.syntax.show._
      implicit val showAnyFromToString: Show[Any] = Show.fromToString
      for {
        init <- State.get[Uml.Graph]
        (updatedGraph: Uml.Graph, newNodes: NewNodes) = fa match {
          case ExpAlg.Pure(name, _) =>
            val node = Uml.LabeledNode(name)
            (init.addNode(node), node :: Nil)
          case ExpAlg.Select(selector, sub) =>
            val (subGraph, startNodes) = sub.foldMap(this).run(init).value
            // TODO: How to visualize selector?
            (subGraph, startNodes)
          case ExpAlg.ForAll(_, sub, _, _) =>
            val newNode = Uml.LabeledNode("FORALL")
            loopSubExpressionAndAddEdges(init, sub, newNode)
          case ExpAlg.Exists(_, sub, _, _) =>
            val newNode = Uml.LabeledNode("EXISTS")
            loopSubExpressionAndAddEdges(init, sub, newNode)
          case ExpAlg.Within(window, _, _) =>
            // TODO: Add links to Stop node for condition node
            val newNode = Uml.LabeledNode(s"WITHIN: ${window.show}")
            (init.addNode(newNode), newNode :: Nil)
          case ExpAlg.Collect(subtypeName, _, sub, _) =>
            val newNode = Uml.LabeledNode(s"WITH TYPE: $subtypeName")
            loopSubExpressionAndAddEdges(init, sub, newNode)
          case _ =>
            (init, Nil)
        }
        _ <- State.set(updatedGraph)
      } yield newNodes
    }

    def apply[T](): ExpAlg[T, *] ~> GraphState = this.asInstanceOf[ExpAlg[T, *] ~> GraphState]
  }
}

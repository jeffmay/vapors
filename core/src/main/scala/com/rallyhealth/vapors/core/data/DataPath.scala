package com.rallyhealth.vapors.core.data

import cats.Show

import scala.annotation.tailrec
import scala.collection.BitSet

// TODO: Better data structure for appending?
final case class DataPath(nodes: List[DataPath.Node]) extends AnyVal {
  import DataPath._

  def apply(idx: Int): DataPath = atIndex(idx)

  def apply(
    startIdx: Int,
    endIdx: Int,
  ): DataPath = slice(startIdx, endIdx)

  def indexes(at: BitSet): DataPath = DataPath(nodes ::: IdxSet(at) :: Nil)

  def slice(
    startIdx: Int,
    endIdx: Int,
  ): DataPath = DataPath(nodes ::: IdxSlice(startIdx, endIdx) :: Nil)

  def between(idxRange: Range): DataPath = DataPath(nodes ::: IdxRange(idxRange) :: Nil)

  def atIndex(idx: Int): DataPath = DataPath(nodes ::: Idx(idx) :: Nil)

  def atHead: DataPath = DataPath(nodes ::: Head :: Nil)

  def atLast: DataPath = DataPath(nodes ::: Last :: Nil)

  def atKey[K : ValidDataPathKey](key: K): DataPath =
    DataPath(nodes ::: MapKey(ValidDataPathKey[K].stringify(key)) :: Nil)

  def atField(name: String): DataPath = DataPath(nodes ::: Field(name) :: Nil)

  def :::(that: DataPath): DataPath = DataPath(this.nodes ::: that.nodes)
}

object DataPath {

  val empty: DataPath = DataPath(Nil)

  sealed trait Node

  // TODO: Make subclasses private for binary compatibility
  final case class Field(name: String) extends Node

  final case class MapKey(key: String) extends Node

  final case object Head extends Node

  final case object Last extends Node

  final case class Idx(index: Int) extends Node

  final case class IdxSlice(
    startIdx: Int,
    endIdx: Int,
  ) extends Node

  final case class IdxRange(range: Range) extends Node

  final case class IdxSet(indexSet: BitSet) extends Node

  @tailrec private def writeToBuilder(
    buffer: StringBuilder,
    path: List[DataPath.Node],
  ): StringBuilder = path match {
    case Nil => buffer
    case head :: tail =>
      var remaining: List[DataPath.Node] = tail
      head match {
        case MapKey(key) =>
          buffer.append('[').append(key).append(']')
        case Field(name) =>
          buffer.append('.').append(name)
        case Head =>
          remaining ::= Idx(0)
        case Last =>
          remaining ::= Idx(-1)
        case Idx(idx) =>
          buffer.append('[').append(idx).append(']')
        case IdxSlice(startIdx, endIdx) =>
          buffer.append('[').append(startIdx).append(':').append(endIdx).append(']')
        case IdxRange(range) if range.step == 1 =>
          remaining ::= IdxSlice(range.start, range.end)
        case IdxRange(range) =>
          remaining ::= IdxSet(BitSet.fromSpecific(range))
        case IdxSet(idxSet) =>
          buffer.append('[')
          for (idx <- idxSet) {
            buffer.append(idx)
            buffer.append(',')
          }
          if (idxSet.nonEmpty) {
            buffer.setLength(buffer.length() - 1)
          }
      }
      writeToBuilder(buffer, remaining)
  }

  implicit val show: Show[DataPath] = Show.show { path =>
    DataPath.writeToBuilder(new StringBuilder(), path.nodes).result()
  }
}

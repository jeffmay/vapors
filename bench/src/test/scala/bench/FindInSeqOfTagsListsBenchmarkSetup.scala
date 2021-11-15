package com.rallyhealth.vapors

package bench

import bench.timeit.Benchmark
import v1.data.FactTable
import v1.dsl.simple.OP
import v1.example.{CombinedTags, FactTypes}

import org.scalacheck.Gen
import org.scalacheck.ops._

import scala.collection.immutable.SortedSet
import scala.util.Random

trait FindInSeqOfTagsListsBenchmarkSetup {

  protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Boolean, OP]): Benchmark

  final lazy val benchmarkFindInSeqOfTagsLists: Benchmark =
    findInSeqOfTagsLists(FindInSeqOfTagsListsBenchmarkSetup.setupDefaultFindInSeqOfTagsLists)
}

object FindInSeqOfTagsListsBenchmarkSetup {
  import v1.dsl.simple._

  // TODO: Make number of runs configurable?
  val setupDefaultFindInSeqOfTagsLists: VaporsBenchmarkSetup[Boolean, OP] =
    setupFindInSeqOfTagsLists(30, 10, 50)

  def setupFindInSeqOfTagsLists(
    numExpressions: Int,
    numTagFacts: Int,
    numTagsPerFact: Int,
  )(implicit
    gc: GenConfig,
  ): VaporsBenchmarkSetup[Boolean, OP] = {
    val expectedTag = "waldo"
    val genTags = Gen.containerOfN[Set, String](numTagsPerFact, Gen.identifier)
    val genTagSets = Gen.listOfN(numTagFacts, genTags)
    val tagsetsWithoutExpected = genTagSets.head
    val lastTagsetWithExpected = tagsetsWithoutExpected.head.drop(1) + expectedTag
    val tagsetsWithExpected = lastTagsetWithExpected :: tagsetsWithoutExpected.tail
    val facts = tagsetsWithExpected.zipWithIndex.map {
      case (tagset, idx) =>
        FactTypes.CombinedTags(CombinedTags(SortedSet.from(tagset), idx + 1))
    }
    VaporsBenchmarkSetup(
      s"[x$numExpressions] find tag in $numTagFacts tag facts ($numTagsPerFact tags each)",
      FactTable(facts),
    ).repeated(numExpressions) {
        valuesOfType(FactTypes.CombinedTags).exists { tags =>
          tags.get(_.select(_.tags)).exists { tag =>
            tag === expectedTag.const
          }
        }
      }
      .ensuringResultIsTrue
  }
}

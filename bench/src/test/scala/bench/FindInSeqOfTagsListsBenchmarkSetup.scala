package com.rallyhealth.vapors

package bench

import bench.timeit.{Benchmark, BenchmarkConfig}
import v1.data.FactTable
import v1.dsl.uncached._ // we only care about the build expressions DSL, so this is safe to share with cached DSL
import v1.example.{CombinedTags, FactTypes}

import org.scalacheck.Gen
import org.scalacheck.ops._

import scala.collection.immutable.SortedSet

trait FindInSeqOfTagsListsBenchmarkSetup {
  import FindInSeqOfTagsListsBenchmarkSetup._

  protected def findInSeqOfTagsLists(setup: VaporsBenchmarkSetup[Params, Boolean, OP]): Benchmark[Params]

  final lazy val benchmarkDefaultFindInSeqOfTagsLists: Benchmark[Params] =
    findInSeqOfTagsLists(FindInSeqOfTagsListsBenchmarkSetup.setupDefaultFindInSeqOfTagsLists)

  final lazy val benchmarkMultiFindInSeqOfTags: Seq[Benchmark[Params]] = Seq(
    Params(1, 10, 10),
    Params(2, 10, 10),
    Params(3, 10, 10),
    Params(4, 10, 10),
    Params(5, 10, 10),
    Params(6, 10, 10),
    Params(7, 10, 10),
    Params(8, 10, 10),
    Params(9, 10, 10),
    Params(10, 10, 10),
  ).map { params =>
    findInSeqOfTagsLists(setupFindInSeqOfTagsLists(params))
  }
}

object FindInSeqOfTagsListsBenchmarkSetup {

  final case class Params(
    numExpressions: Int,
    numTagFacts: Int,
    numTagsPerFact: Int,
  )

  // TODO: Make number of runs configurable?
  val setupDefaultFindInSeqOfTagsLists: VaporsBenchmarkSetup[Params, Boolean, OP] =
    setupFindInSeqOfTagsLists(Params(30, 10, 50))

  def setupFindInSeqOfTagsLists(
    params: Params,
  )(implicit
    gc: GenConfig,
  ): VaporsBenchmarkSetup[Params, Boolean, OP] = {
    val expectedTag = "waldo"
    val genTags = Gen.containerOfN[Set, String](params.numTagsPerFact, Gen.identifier)
    val genTagSets = Gen.listOfN(params.numTagFacts, genTags)
    val tagsetsWithoutExpected = genTagSets.head
    val lastTagsetWithExpected = tagsetsWithoutExpected.head.drop(1) + expectedTag
    val tagsetsWithExpected = lastTagsetWithExpected :: tagsetsWithoutExpected.tail
    val facts = tagsetsWithExpected.zipWithIndex.map {
      case (tagset, idx) =>
        FactTypes.CombinedTags(CombinedTags(SortedSet.from(tagset), idx + 1))
    }
    VaporsBenchmarkSetup(
      BenchmarkConfig(
        s"[x${params.numExpressions}] find tag in ${params.numTagFacts} tag facts (${params.numTagsPerFact} tags each)",
        params,
      ),
      FactTable(facts),
    ).repeated(params.numExpressions) {
        valuesOfType(FactTypes.CombinedTags).exists {
          _.getAs[Seq](_.select(_.tags)).exists { tag =>
            tag === expectedTag.const
          }
        }
      }
      .ensuringResultIsTrue
  }
}

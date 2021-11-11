package com.rallyhealth.vapors.v1

import data.FactTable
import engine.SimpleCachingEngine
import example.{CombinedTags, FactTypes}

import munit.FunSuite

import scala.collection.immutable.SortedSet

class CachingSpec extends FunSuite {

  import dsl.simple._

  protected def timeit[T](
    name: String,
    iterations: Int = 1000,
    warmupIterations: Int = 1000,
  )(
    body: => T,
  ): T = {
    val result: T = body
    for (_ <- 1 until warmupIterations) {
      body
    }
    val durationNanos = for (_ <- 1 to iterations) yield {
      val start = System.nanoTime()
      body
      val end = System.nanoTime()
      end - start
    }
    val durationMin = durationNanos.min / 1000.0
    val durationMax = durationNanos.max / 1000.0
    val durationAvg = durationNanos.sum / durationNanos.size / 1000.0
    println(s"Timeit: $name")
    println(s"Duration (ms): MIN=$durationMin, MAX=$durationMax, AVG=$durationAvg")
    result
  }

  test("caching performance") {
    val allTags =
      Seq.fill(10)(Seq("A", "B", "C")).map(abcs => FactTypes.CombinedTags(CombinedTags(SortedSet.from(abcs), 1)))
    val seqOfSeqs = valuesOfType(FactTypes.CombinedTags).exists { tags =>
      tags.get(_.select(_.tags)).exists { tag =>
        tag === "C".const
      }
    }
    val (result, cache) = timeit("seqOfSeqs-caching") {
      SimpleCachingEngine.debugMultiple(Seq.fill(10)(seqOfSeqs), FactTable(allTags))
    }
    println(s"CACHE (${cache.size}): $cache")
    timeit("seqOfSeqs-nocache") {
      Seq.fill(10) {
        seqOfSeqs.run(FactTable(allTags))
      }
    }
  }
}

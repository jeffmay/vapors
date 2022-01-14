package com.rallyhealth.vapors.v1

package testutil

import org.scalactic.source.Position

import java.time.temporal.ChronoUnit
import java.time.{Clock, Instant, ZoneId, ZoneOffset}
import java.util.concurrent.TimeUnit
import scala.annotation.switch
import scala.collection.immutable.NumericRange
import scala.concurrent.duration._

/**
  * A [[Clock]] that ticks along a pre-defined path.
  *
  * Every call to [[instant()]] iterates through the list of pre-defined moments and adds the [[Instant]]
  * returned to the [[returned]] sequence. It is possible that a limited number moments have been defined,
  * in which case, getting the next moment can throw a [[NoSuchElementException]].
  *
  * @note You can [[readOption()]] the next moment, but keep in mind that this will compute the moment
  *       and fix the result to be the next moment that is returned.
  *
  * @param moments the finite or infinite list of moments to return
  * @param zone the [[ZoneId]] used by any operations that use the clock for its time zone information
  * @param definedAt where this instance of the [[TestClock]] was defined for debugging purposes
  */
final class TestClock private (
  val moments: LazyList[Instant],
  private var zone: ZoneId,
  definedAt: Position,
) extends Clock {

  @volatile private var returnedMoments: Vector[(StackTraceElement, Instant)] = Vector()

  /**
    * The list of moments returned by [[instant()]] so far and where they was called from.
    */
  def returnedWithTrace: IndexedSeq[(StackTraceElement, Instant)] = returnedMoments

  /**
    * The list of moments returned by [[instant()]] so far.
    */
  def returned: IndexedSeq[Instant] = returnedMoments.map(_._2)

  @volatile private var remainingMoments: LazyList[Instant] = moments

  /**
    * The lazy list of remaining moments to be returned when calling [[instant()]].
    */
  def remaining: LazyList[Instant] = remainingMoments

  /**
    * Returns a string containing all instants returned by this clock in the order in which it
    * was invoked including the source code location where it was invoked, separated by newlines.
    *
    * @param fmtInstant a format function for serializing the instant (defaults to {number}ms)
    */
  def formatAllInvocations(
    indent: Int = 0,
    fmtInstant: Instant => String = m => s"${m.toEpochMilli}ms",
  ): String = {
    val sb = new StringBuilder
    for ((trace, instant) <- returnedMoments)
      sb.append(s"${" " * indent}${fmtInstant(instant)} : $trace\n")
    sb.result()
  }

  override def getZone: ZoneId = zone

  override def withZone(zone: ZoneId): Clock = {
    this.zone = zone
    this
  }

  /**
    * Lazily compute the next moment to return, add it to the [[returned]] sequence, set the [[remaining]] list to
    * the tail of the moments list, and then return the computed moment.
    *
    * @throws NoSuchElementException if the moments list is finite and all of them have been returned
    * @return the next computed moment
    */
  @throws[NoSuchElementException]("The TestClock has a finite list of moments and all of them have been returned.")
  override def instant(): Instant = this.synchronized {
    val head = readOrThrow()
    val trace = new IllegalArgumentException().getStackTrace
    remainingMoments = remainingMoments.tail
    returnedMoments :+= (trace(1), head)
    head
  }

  /**
    * Compute the next moment to be returned, but don't iterate through it.
    *
    * @return the next moment to be returned when calling the [[instant()]] method.
    */
  @throws[NoSuchElementException]("The TestClock has a finite list of moments and all of them have been returned.")
  def readOrThrow(): Instant = remainingMoments.headOption.getOrElse {
    throw new NoSuchElementException(
      s"TestClock (from ${PositionUtils.stringify(definedAt)}) only defined with ${remainingMoments.size} moments.",
    )
  }

  /**
    * Compute the next moment to be returned, but don't iterate through it.
    *
    * @return the next moment to be returned when calling the [[instant()]] method or None if at the end of the list.
    */
  def readOption(): Option[Instant] = remainingMoments.headOption

  override def toString: String = {
    s"TestClock (defined at ${PositionUtils.stringify(definedAt)}) {\n  invocations = [\n${formatAllInvocations(4)}  ]\n}"
  }
}

object TestClock {

  /**
    * A [[TestClock]] that computes the next moment with [[Instant.now()]].
    *
    * This is the same behavior as the default [[Clock]], except that the result of all [[TestClock.instant()]]
    * calls are stored in the [[TestClock.returned]] sequence for future validation.
    */
  def systemUTC()(implicit pos: Position): TestClock = TestClock.continually(Instant.now())

  @inline def apply(
    moments: LazyList[Instant],
    zone: ZoneId,
  )(implicit
    pos: Position,
  ): TestClock = {
    new TestClock(moments, zone, pos)
  }

  @inline def apply(moments: LazyList[Instant])(implicit pos: Position): TestClock = {
    new TestClock(moments, ZoneOffset.UTC, pos)
  }

  /**
    * A [[TestClock]] that lazily computes the next [[Instant]] to be returned.
    */
  def continually(compute: => Instant)(implicit pos: Position): TestClock = {
    TestClock(LazyList.continually(compute))
  }

  /**
    * A [[TestClock]] that always returns the given [[Instant]].
    */
  def fixed(moment: Instant)(implicit pos: Position): TestClock = {
    TestClock(LazyList.continually(moment))
  }

  /**
    * A [[TestClock]] that returns the given [[Instant]]s in the order given.
    *
    * After all moments have been exhausted, this clock will throw an exception.
    */
  def stub(moments: Instant*)(implicit pos: Position): TestClock = {
    TestClock(LazyList.from(moments))
  }

  /**
    * A [[TestClock]] that returns the given moments (as millis from the Epoch) in the order given.
    *
    * After all moments have been exhausted, this clock will throw an exception.
    */
  def stubMillis(moments: Long*)(implicit pos: Position): TestClock = {
    TestClock(LazyList.from(moments).map(Instant.ofEpochMilli))
  }

  /**
    * A [[TestClock]] that returns the given moments (as seconds from the Epoch) in the order given.
    *
    * After all moments have been exhausted, this clock will throw an exception.
    */
  def stubSeconds(moments: Long*)(implicit pos: Position): TestClock = {
    TestClock(LazyList.from(moments).map(Instant.ofEpochSecond))
  }

  @deprecated("Replaced by stubRangeMillis", "v8.2.0")
  def stubRange(range: NumericRange[Long])(implicit pos: Position): TestClock = {
    TestClock(LazyList.from(range).map(Instant.ofEpochMilli))
  }

  /**
    * A [[TestClock]] that returns a range of moments (as seconds from the Epoch) in increasing order.
    *
    * After all moments have been exhausted, this clock will throw an exception.
    */
  def stubRangeSeconds(range: NumericRange[Long])(implicit pos: Position): TestClock = {
    TestClock(LazyList.from(range).map(Instant.ofEpochSecond))
  }

  /**
    * A [[TestClock]] that returns a range of moments (as milliseconds from the Epoch) in increasing order.
    *
    * After all moments have been exhausted, this clock will throw an exception.
    */
  def stubRangeMillis(range: NumericRange[Long])(implicit pos: Position): TestClock = {
    TestClock(LazyList.from(range).map(Instant.ofEpochMilli))
  }

  private def addNanosPerTickFrom(
    start: Instant,
    tickAmountNanos: Long,
  )(implicit
    pos: Position,
  ): TestClock = {
    var next: Instant = start
    TestClock(LazyList.continually {
      val current = next
      next = next.plusNanos(tickAmountNanos)
      current
    })
  }

  /**
    * A [[TestClock]] that steps forward the given [[Duration]] per call to [[TestClock.instant()]]
    * relative to the given [[Instant]].
    */
  @inline def addDurationPerTickFrom(
    start: Instant,
    tickAmount: Duration,
  )(implicit
    pos: Position,
  ): TestClock = {
    addNanosPerTickFrom(start, tickAmount.toNanos)
  }

  /**
    * A [[TestClock]] that steps forward the given [[Duration]] per call to [[TestClock.instant()]]
    * relative to [[Instant.now()]] (called when this is constructed).
    */
  @inline def addDurationPerTickFromNow(tickAmount: Duration)(implicit pos: Position): TestClock = {
    addNanosPerTickFrom(Instant.now(), tickAmount.toNanos)
  }

  /**
    * A [[TestClock]] that steps forward 1 [[TimeUnit]] per call to [[TestClock.instant()]]
    * relative to the start of the current [[TimeUnit]] when this is constructed.
    */
  def oneUnitPerTickFromNow(tickUnit: TimeUnit)(implicit pos: Position): TestClock = {
    val chronoUnit = (tickUnit.ordinal(): @switch) match {
      case 0 => ChronoUnit.NANOS
      case 1 => ChronoUnit.MICROS
      case 2 => ChronoUnit.MILLIS
      case 3 => ChronoUnit.SECONDS
      case 4 => ChronoUnit.MINUTES
      case 5 => ChronoUnit.HOURS
      case 6 => ChronoUnit.DAYS
      case _ => throw new MatchError(s"Unrecognized TimeUnit: ${tickUnit}")
    }
    val start = Instant.now().truncatedTo(chronoUnit)
    addNanosPerTickFrom(start, chronoUnit.getDuration.toNanos)
  }

  /**
    * A [[TestClock]] that steps forward 1 millisecond per call to [[TestClock.instant()]]
    * relative to the start of the current millisecond when this is constructed.
    */
  @inline def oneMilliPerTickFromNow()(implicit pos: Position): TestClock = {
    oneUnitPerTickFromNow(MILLISECONDS)
  }

  /**
    * A [[TestClock]] that steps forward 1 second per call to [[TestClock.instant()]]
    * relative to the start of the current second when this is constructed.
    */
  @inline def oneSecondPerTickFromNow()(implicit pos: Position): TestClock = {
    oneUnitPerTickFromNow(SECONDS)
  }
}

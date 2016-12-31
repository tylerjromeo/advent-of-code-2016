package org.romeo.adventofcode.day20

import org.romeo.adventofcode.common.Puzzle

import scala.collection.immutable.NumericRange
import scala.collection.mutable.ArrayBuffer

/**
  * Created by tylerromeo on 12/30/16.
  */
object DayTwenty {
  def main(args: Array[String]): Unit = {
    new DayTwenty().run()
  }

  def parseInput(input: String): List[Range] = {
    input.split("\n").map(s => {
      val split = s.split("-")
      Range(split(0).toLong, split(1).toLong)
    }).toList
  }
}

case class Range(min: Long, max: Long) {
  def contains(n: Int): Boolean = {
    min <= n && n <= max
  }
}

class DayTwenty extends Puzzle("http://adventofcode.com/2016/day/20/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayTwenty.parseInput(input).forall(r => {
      r.min >= 0 && r.min <= 4294967295L &&
      r.max >= 0 && r.max <= 4294967295L
    }))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
override def solvePart1(input: String): String = {
  val blackList = DayTwenty.parseInput(input)
  Stream.from(0).find(n => !blackList.exists(_.contains(n))).get.toString
}

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

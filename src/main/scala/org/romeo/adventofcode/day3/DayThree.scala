package org.romeo.adventofcode.day3

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/8/16
  * Time: 10:05 AM
  *
  */
object DayThree {
  def main(args: Array[String]): Unit = {
    new DayThree().run()
  }

  def splitInput(input: String): List[List[Int]] = {
    input.trim.split("\n").map(_.trim.split("\\s+")
      .map(_.toInt)
      .toList).toList
  }

  /**
    * given a list containing 3 side sizes, return true if it is a valid triang;e
    *
    * @param sides
    */
  def isTriangle(sides: List[Int]): Boolean = {
    sides(0) + sides(1) > sides(2) &&
      sides(1) + sides(2) > sides(0) &&
      sides(2) + sides(0) > sides(1)
  }
}

class DayThree extends Puzzle("http://adventofcode.com/2016/day/3/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayThree.splitInput(input).forall(_.size == 3))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    DayThree.splitInput(input).count(DayThree.isTriangle).toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

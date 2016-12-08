package org.romeo.adventofcode.day2

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/7/16
  * Time: 11:52 PM
  *
  */
object DayTwo {
  def splitInput(input: String): List[String] = input.split("\n").toList

  def main(args: Array[String]): Unit = {
    new DayTwo().run()
  }
}

class DayTwo extends Puzzle("http://adventofcode.com/2016/day/2/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayTwo.splitInput(input).forall(_.forall(c => c == 'U' || c == 'R' || c == 'L' || c == 'D')))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
override def solvePart1(input: String): String = ???

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

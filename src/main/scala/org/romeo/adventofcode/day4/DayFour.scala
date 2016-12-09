package org.romeo.adventofcode.day4

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/8/16
  * Time: 2:07 PM
  *
  */
object DayFour {
  def main(args: Array[String]): Unit = {
    new DayFour().run()
  }

  def parseInput(input: String): List[Room] = {
    input.trim.split("\n").map(s => {
      val split = s.split("""\[|\]""")
      val segments = split(0).split("-").reverse
      val name = segments.tail.reduce(_ + _)
      Room(name, segments.head.toInt, split(1))
    }).toList
  }
}

case class Room(name: String, id: Int, checksum: String)

class DayFour extends Puzzle("http://adventofcode.com/2016/day/4/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayFour.parseInput(input).forall(r => !r.name.isEmpty && r.checksum.length == 5))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    ""
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

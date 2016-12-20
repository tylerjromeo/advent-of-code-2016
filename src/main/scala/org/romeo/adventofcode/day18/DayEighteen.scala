package org.romeo.adventofcode.day18

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/20/16
  * Time: 4:27 PM
  *
  */
object DayEighteen {
  def main(args: Array[String]): Unit = {
    new DayEighteen().run()
  }

  def nextRow(row: String): String = {
    val paddedRow = "." + row + "."
    paddedRow.sliding(3).map(buildTile).mkString
  }

  def buildTile(s: String): Char = {
    (s(0), s(1), s(2)) match {
      case ('^', '^', '.') => '^'
      case ('.', '^', '^') => '^'
      case ('^', '.', '.') => '^'
      case ('.', '.', '^') => '^'
      case _ => '.'
    }

  }
}

class DayEighteen extends Puzzle("http://adventofcode.com/2016/day/18/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    input.forall(c => c == '.' || c == '^')
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val rows = Stream.iterate(input)(DayEighteen.nextRow).take(40)
    rows.foldLeft(0) {
      case (count, row) => count + row.count(_ == '.')
    }.toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val rows = Stream.iterate(input)(DayEighteen.nextRow).take(400000)
    rows.foldLeft(0) {
      case (count, row) => count + row.count(_ == '.')
    }.toString
  }
}

package org.romeo.adventofcode.day13

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/14/16
  * Time: 9:18 PM
  *
  */
object DayThirteen {
  def main(args: Array[String]): Unit = {
    new DayThirteen().run()
  }

  def spaceFunction(magicNumber: Int): (Int, Int) => Space = (x: Int, y: Int) => {
    if (x < 0 || y < 0) {
      Wall
    } else {
      val n = x * x + 3 * x + 2 * x * y + y + y * y + magicNumber
      if (n.toBinaryString.count(_ == '1') % 2 == 0) {
        Open
      } else {
        Wall
      }
    }
  }

  def validMoves(f: (Int, Int) => Space, coords: (Int, Int)): Set[(Int, Int)] = {
    val (x, y) = coords
    Set(
      (x-1, y),
      (x+1, y),
      (x, y+1),
      (x, y-1)
    ).filter(f.tupled(_) == Open)
  }
}

sealed trait Space

case object Open extends Space

case object Wall extends Space

class DayThirteen extends Puzzle("http://adventofcode.com/2016/day/13/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(input.forall(_.isDigit))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val startCoords = (1, 1)
    val targetCoords = (31, 39)
    val spaceFunction = DayThirteen.spaceFunction(input.toInt)

    var count = 0
    var checkedPaths = scala.collection.mutable.Set[(Int, Int)]()
    var paths = Set(startCoords)
    var matchFound = false
    while(!matchFound){
      count = count + 1
      checkedPaths ++= paths
      paths = paths.flatMap(DayThirteen.validMoves(spaceFunction, _)) -- checkedPaths
      matchFound = paths.contains(targetCoords)
    }
    count.toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

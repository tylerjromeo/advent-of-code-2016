package org.romeo.adventofcode.day15

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/16/16
  * Time: 9:06 PM
  *
  */
object DayFifteen {
  def main(args: Array[String]): Unit = {
    new DayFifteen().run()
  }

  def parseInput(input: String): List[Disc] = {
    input.split("\n").map(s => {
      val split = s.split(" ")
      Disc(split(3).toInt, split(11).dropRight(1).toInt)
    }).toList
  }
}

case class Disc(nPositions: Int, zeroPosition: Int) {
  def positionForTime(time: Int): Int = {
    (zeroPosition + time) % nPositions
  }
}

class DayFifteen extends Puzzle("http://adventofcode.com/2016/day/15/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayFifteen.parseInput(input).forall(_.isInstanceOf[Disc]))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val discs = DayFifteen.parseInput(input)
    def discFunctions(time: Int): List[Int] = {
      discs.zipWithIndex.map {
        case (disc, i) => disc.positionForTime (time + i + 1)
      }
    }
//    discFunctions(16824).mkString("\n")
    Stream.from(0).map(discFunctions).zipWithIndex.find{
      case (l, _) => l.forall(_ == 0)
    }.get._2.toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val discs = DayFifteen.parseInput(input) ::: List(Disc(11, 0))
    def discFunctions(time: Int): List[Int] = {
      discs.zipWithIndex.map {
        case (disc, i) => disc.positionForTime (time + i + 1)
      }
    }
    //    discFunctions(16824).mkString("\n")
    Stream.from(0).map(discFunctions).zipWithIndex.find{
      case (l, _) => l.forall(_ == 0)
    }.get._2.toString
  }
}

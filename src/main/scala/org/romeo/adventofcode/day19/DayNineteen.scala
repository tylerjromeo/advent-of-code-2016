package org.romeo.adventofcode.day19

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/24/16
  * Time: 8:36 PM
  *
  */
object DayNineteen {
  def main(args: Array[String]): Unit = {
    new DayNineteen().run()
  }

  def performRound(start: List[Elf]): List[Elf]= {
    start.grouped(2).foldLeft(List[Elf]()){
      case (acc, elves) => {
        // last elf in list, take from elf 1
        if(elves.size == 1) {
          elves(0).stealFrom(acc.last) :: acc.dropRight(1)
        } else {
          elves(0).stealFrom(elves(1)) :: acc
        }
      }
    }.reverse
  }
}

case class Elf(name: String, giftCount: Int) {
  def stealFrom(victim: Elf): Elf = {
    Elf(name, giftCount + victim.giftCount)
  }
}

class DayNineteen extends Puzzle("http://adventofcode.com/2016/day/19/input") {
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
  val numElves = input.toInt
  val elves = Stream.from(1).map(i => Elf(i.toString, 1)).take(numElves)

  var survivors = elves.toList
  while(survivors.size > 1) {
    survivors = DayNineteen.performRound(survivors)
  }
  survivors.head.name
}

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

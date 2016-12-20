package org.romeo.adventofcode.day16

import org.romeo.adventofcode.common.Puzzle

import scala.annotation.tailrec

/**
  * User: tylerromeo
  * Date: 12/19/16
  * Time: 9:08 AM
  *
  */
object DaySixteen {
  def main(args: Array[String]): Unit = {
    new DaySixteen().run()
  }

  def dragonCurve(a: String): String = {
    a + '0' + a.reverse.map {
      case '0' => '1'
      case '1' => '0'
    }
  }

  @tailrec
  def checksum(s: String): String = {
    val retval = s.grouped(2).map(s => {
      if (s(0) == s(1)) {
        '1'
      } else {
        '0'
      }
    }).mkString
    if (retval.length % 2 == 1) retval else checksum(retval)
  }
}


class DaySixteen extends Puzzle("http://adventofcode.com/2016/day/16/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(input.forall(c => c == '0' || c == '1'))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val diskSize = 272
    var pad = input
    while (pad.length < diskSize) {
      pad = DaySixteen.dragonCurve(pad)
    }
    DaySixteen.checksum(pad.take(diskSize))
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val diskSize = 35651584
    var pad = input
    while (pad.length < diskSize) {
      pad = DaySixteen.dragonCurve(pad)
    }
    DaySixteen.checksum(pad.take(diskSize))
  }
}

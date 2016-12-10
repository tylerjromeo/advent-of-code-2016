package org.romeo.adventofcode.day7

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/9/16
  * Time: 8:41 PM
  *
  */
object DaySeven {
  def main(args: Array[String]): Unit = {
    new DaySeven().run()
  }

  def isAbba(s: String): Boolean = {
    s.length == 4 &&
      s(0) == s(3) &&
      s(1) == s(2) &&
      s(0) != s(1)
  }

  def parseIp(ip: String): Ip = {
    var nonBrackets = List[String]()
    var brackets = List[String]()
    ip.split("""\[|\]""").zipWithIndex.foreach { case ((s, i)) => {
      if (i % 2 == 0) {
        nonBrackets = s :: nonBrackets
      } else {
        brackets = s :: brackets
      }
    }
    }
    Ip(nonBrackets, brackets)
  }

  def supportsTls(ip: Ip): Boolean = {
    ip.bracket.flatMap(_.sliding(4)).forall(!isAbba(_)) &&
      ip.nonBracket.flatMap(_.sliding(4)).exists(isAbba)
  }

  def supportsTlsString = supportsTls _ compose parseIp
}

case class Ip(nonBracket: List[String], bracket: List[String])

class DaySeven extends Puzzle("http://adventofcode.com/2016/day/7/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    // Do nothing
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val ips = input.trim.split("\n")
    ips.count(DaySeven.supportsTlsString).toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

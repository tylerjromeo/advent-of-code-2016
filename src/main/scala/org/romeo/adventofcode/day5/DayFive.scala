package org.romeo.adventofcode.day5

import java.security.MessageDigest

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/8/16
  * Time: 9:04 PM
  *
  */
object DayFive {
  def main(args: Array[String]): Unit = {
    new DayFive().run()
  }

  def hash(src: String): String = {
    MessageDigest.getInstance("MD5").digest(src.getBytes).map("%02X" format _).mkString
  }
}

class DayFive extends Puzzle("http://adventofcode.com/2016/day/5/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(!input.isEmpty)
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val specialPrefix = "00000"
    val matchingHashes = Stream.from(0).map(n => DayFive.hash(input + n))
      .filter(_.startsWith(specialPrefix))

    println("This could take a minute, hold on")
    matchingHashes.take(8).map(_.charAt(5)).mkString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val specialPrefix = "00000"
    val matchingHashes = Stream.from(0).map(n => DayFive.hash(input + n))
      .filter(_.startsWith(specialPrefix))

    println("This could take a minute, hold on")
  }
}

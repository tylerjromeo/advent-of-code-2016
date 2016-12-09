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

  def calculateChecksum(name: String): String = {
    val characterCounts = name.foldLeft(Map[Char, Int]())((acc, c) => {
      acc + (c -> (acc.getOrElse(c, 0) + 1))
    })
    characterCounts.toList.sortWith {
      case ((c1, i1), (c2, i2)) => if (i1 == i2) c1 < c2 else i1 > i2
    }.foldLeft("")(_ + _._1).take(5)
  }

  def decryptRoomName(name: String, rotCypher: Int): String = {
    val realCypher = rotCypher % 26
    name.map(rotateChar(_, realCypher))
  }

  def rotateChar(char: Char, n: Int): Char = {
    val newChar = (char + n).toChar
    if (newChar > 'z')
      (newChar - 26).toChar
    else
      newChar
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
    DayFour.parseInput(input).filter(r => {
      r.checksum == DayFour.calculateChecksum(r.name)
    }).foldLeft(0)(_ + _.id).toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val targets = List("north", "pole", "object", "objects")
    val matches = DayFour.parseInput(input).filter(room => {
      targets.exists(t => DayFour.decryptRoomName(room.name, room.id).contains(t))
    })
    s"Possible matches are: $matches\ntry ids: ${matches.map(_.id)}"
  }
}

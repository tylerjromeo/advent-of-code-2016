package org.romeo.adventofcode.day8

import org.romeo.adventofcode.common.Puzzle

import scala.collection.mutable

/**
  * User: tylerromeo
  * Date: 12/10/16
  * Time: 1:22 PM
  *
  */
object DayEight {
  def main(args: Array[String]): Unit = {
    new DayEight().run()
  }

  def parseInstruction(input: String): Instruction = {
    if (input.startsWith("rect")) {
      val coords = input.split(" ")(1).split("x")
      Rect(coords(0).toInt, coords(1).toInt)
    } else if (input.startsWith("rotate")) {
      val rotation = input.split(" ")
      rotation(1) match {
        case "row" => RotateRow(rotation(2).substring(2).toInt, rotation(4).toInt)
        case "column" => RotateColumn(rotation(2).substring(2).toInt, rotation(4).toInt)
        case _ => Rect(0, 0) // no op
      }
    } else {
      // no op
      Rect(0, 0)
    }
  }
}

trait Instruction

case class Rect(width: Int, height: Int) extends Instruction

case class RotateRow(rowNum: Int, moves: Int) extends Instruction

case class RotateColumn(colNum: Int, moves: Int) extends Instruction

class Screen(width: Int, height: Int) {
  //grid representing which pixels are turned ON
  private val pixels = mutable.MutableList.fill(height)(mutable.MutableList.fill(width)(false))

  private def drawRect(width: Int, height: Int) = {
    for (h <- 0 until height; w <- 0 until width) {
      pixels(h)(w) = true
    }
  }

  private def rotateRow(rowNum: Int, moves: Int) = {
    val row = pixels(rowNum)
    pixels(rowNum) = row.takeRight(moves) ++ row.dropRight(moves)
  }

  private def rotateColumn(colNum: Int, moves: Int) = {
    val newColumn = mutable.MutableList.fill(height)(false)
    for (i <- 0 until height) {
      newColumn(i) = pixels((i + (height - moves)) % height)(colNum)
    }
    for (i <- 0 until height) {
      pixels(i)(colNum) = newColumn(i)
    }
  }

  def execute(instruction: Instruction) = instruction match {
    case Rect(w, h) => drawRect(w, h)
    case RotateRow(n, m) => rotateRow(n, m)
    case RotateColumn(n, m) => rotateColumn(n, m)
  }

  override def toString: String = {
    "\n" +
      pixels.map(_.map(if (_) "#" else " ").mkString).mkString("\n")
  }

  def countPixels: Int = {
    pixels.flatten.count(identity)
  }
}

class DayEight extends Puzzle("http://adventofcode.com/2016/day/8/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(input.split("\n").map(DayEight.parseInstruction).forall(_.isInstanceOf[Instruction]))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val screen = new Screen(50, 6)
    input.split("\n").map(DayEight.parseInstruction).foreach(screen.execute)
    screen.countPixels.toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    ""
  }
}

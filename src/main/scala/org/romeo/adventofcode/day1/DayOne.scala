package org.romeo.adventofcode.day1

import org.romeo.adventofcode.common.Puzzle
import org.romeo.adventofcode.day1.DayOne.Person

/**
  * User: tylerromeo
  * Date: 12/7/16
  * Time: 9:30 PM
  *
  */
object DayOne {

  //representing location as an (x,y) tuple. x is left right, y is up down
  sealed trait Direction {
    def move(location: (Int, Int), number: Int): (Int, Int)
  }

  case object Up extends Direction {
    override def move(location: (Int, Int), number: Int): (Int, Int) = (location._1, location._2 + number)
  }

  case object Down extends Direction {
    override def move(location: (Int, Int), number: Int): (Int, Int) = (location._1, location._2 - number)
  }

  case object Left extends Direction {
    override def move(location: (Int, Int), number: Int): (Int, Int) = (location._1 - number, location._2)
  }

  case object Right extends Direction {
    override def move(location: (Int, Int), number: Int): (Int, Int) = (location._1 + number, location._2)
  }

  def turnLeft(direction: Direction): Direction = direction match {
    case Up => Left
    case Left => Down
    case Down => Right
    case Right => Up
  }

  def turnRight(direction: Direction): Direction = direction match {
    case Up => Right
    case Right => Down
    case Down => Left
    case Left => Up
  }

  case class Person(location: (Int, Int) = (0, 0), heading: Direction = Up)

  def main(args: Array[String]): Unit = {
    new DayOne().run()
  }
}

class DayOne extends Puzzle("http://adventofcode.com/2016/day/1/input") {

  override def solvePart1(input: String): String = {

    val directions = input.split(", ").map(_.splitAt(1))
    assert(directions.forall{
      case (turn, number) => {
        (turn == "L" || turn == "R") &&
        number.forall(Character.isDigit)
      }
    })

    val finalLocation = directions.foldLeft[Person](Person()){
      case (p, (turn, num)) => {
//        println(s"$p turning $turn and moving $num")
        val newHeading = turn match {
          case "L" => DayOne.turnLeft(p.heading)
          case "R" => DayOne.turnRight(p.heading)
        }
        Person(location = newHeading.move(p.location, num.toInt), heading = newHeading)
      }
    }

    s"${math.abs(finalLocation.location._1) + math.abs(finalLocation.location._2)}"
  }

  override def solvePart2(input: String): String = ???
}

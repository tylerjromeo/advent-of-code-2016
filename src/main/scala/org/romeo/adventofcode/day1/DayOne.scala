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

  def splitInput(input: String): Seq[(String, String)] = input.split(", ").map(_.splitAt(1))

  // FIXME do it the right tailrec way
  //  @tailrec
  def getAllMoves(person: Person, num: Int): List[Person] = {
    num match {
      case x if x <= 0 => List[Person]()
      case x => {
        val newPerson = Person(location = person.heading.move(person.location, 1), heading = person.heading)
        List[Person](newPerson) ::: getAllMoves(newPerson, num - 1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    new DayOne().run()
  }
}

class DayOne extends Puzzle("http://adventofcode.com/2016/day/1/input") {

  override def validateInput(input: String): Unit = {
    assert(DayOne.splitInput(input).forall {
      case (turn, number) => {
        (turn == "L" || turn == "R") &&
          number.forall(Character.isDigit)
      }
    })
  }

  override def solvePart1(input: String): String = {

    val directions = DayOne.splitInput(input)

    val finalLocation = directions.foldLeft[Person](Person()) {
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

  override def solvePart2(input: String): String = {

    val directions = DayOne.splitInput(input)

    //    lazy val allLocations: Stream[(Person, (String, String))] = directions.flatMap()
    val locations = directions.foldLeft(List(Person())) {
      case (ps, (turn, num)) => {
        //        println(s"$p turning $turn and moving $num")
        val p: Person = ps.last
        val newHeading = turn match {
          case "L" => DayOne.turnLeft(p.heading)
          case "R" => DayOne.turnRight(p.heading)
        }
        ps ::: DayOne.getAllMoves(p.copy(heading = newHeading), num.toInt)
      }
    }

    lazy val allLocations = locations.scanLeft(List[(Int, Int)]())((acc, person) =>
      person.location :: acc
    )

    // scan until we find a duplicate. Once we find one, we know the last element we added (head) is the dupe
    //we're making a leap of faith and assuming the data set has a real answer so we can force unwrap the optional
    val firstDuplicateLocation = allLocations.find(l => l.distinct.size != l.size).get.head
    s"${math.abs(firstDuplicateLocation._1) + math.abs(firstDuplicateLocation._2)}"
  }
}

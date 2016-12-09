package org.romeo.adventofcode.day2

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/7/16
  * Time: 11:52 PM
  *
  */
object DayTwo {
  def splitInput(input: String): List[String] = input.split("\n").toList

  def move(start: Button, direction: Char): Button = direction match {
    case 'U' => start.up
    case 'D' => start.down
    case 'L' => start.left
    case 'R' => start.right
  }

  def main(args: Array[String]): Unit = {
    new DayTwo().run()
  }

  def followDirections(directions: Traversable[String], start: Button): String = {
    directions.foldLeft(("", start)) {
      case ((code, startButton), moves) => {
        val finalPosition = moves.foldLeft(startButton.asInstanceOf[Button])(DayTwo.move)
        (code + finalPosition.digit, finalPosition)
      }
    }._1
  }
}

sealed trait Button {
  val digit: String
  val left: Button
  val right: Button
  val up: Button
  val down: Button
}

object KeypadOne {

  case object One extends Button {
    override val digit: String = "1"
    override val left: Button = One
    override val right: Button = Two
    override val up: Button = One
    override val down: Button = Four
  }

  case object Two extends Button {
    override val digit: String = "2"
    override val left: Button = One
    override val right: Button = Three
    override val up: Button = Two
    override val down: Button = Five
  }

  case object Three extends Button {
    override val digit: String = "3"
    override val left: Button = Two
    override val right: Button = Three
    override val up: Button = Three
    override val down: Button = Six
  }

  case object Four extends Button {
    override val digit: String = "4"
    override val left: Button = Four
    override val right: Button = Five
    override val up: Button = One
    override val down: Button = Seven
  }

  case object Five extends Button {
    override val digit: String = "5"
    override val left: Button = Four
    override val right: Button = Six
    override val up: Button = Two
    override val down: Button = Eight
  }

  case object Six extends Button {
    override val digit: String = "6"
    override val left: Button = Five
    override val right: Button = Six
    override val up: Button = Three
    override val down: Button = Nine
  }

  case object Seven extends Button {
    override val digit: String = "7"
    override val left: Button = Seven
    override val right: Button = Eight
    override val up: Button = Four
    override val down: Button = Seven
  }

  case object Eight extends Button {
    override val digit: String = "8"
    override val left: Button = Seven
    override val right: Button = Nine
    override val up: Button = Five
    override val down: Button = Eight
  }

  case object Nine extends Button {
    override val digit: String = "9"
    override val left: Button = Eight
    override val right: Button = Nine
    override val up: Button = Six
    override val down: Button = Nine
  }

}

object KeypadTwo {

  case object One extends Button {
    override val digit: String = "1"
    override val left: Button = One
    override val right: Button = One
    override val up: Button = One
    override val down: Button = Three
  }

  case object Two extends Button {
    override val digit: String = "2"
    override val left: Button = Two
    override val right: Button = Three
    override val up: Button = Two
    override val down: Button = Six
  }

  case object Three extends Button {
    override val digit: String = "3"
    override val left: Button = Two
    override val right: Button = Four
    override val up: Button = One
    override val down: Button = Seven
  }

  case object Four extends Button {
    override val digit: String = "4"
    override val left: Button = Three
    override val right: Button = Four
    override val up: Button = Four
    override val down: Button = Eight
  }

  case object Five extends Button {
    override val digit: String = "5"
    override val left: Button = Five
    override val right: Button = Six
    override val up: Button = Five
    override val down: Button = Five
  }

  case object Six extends Button {
    override val digit: String = "6"
    override val left: Button = Five
    override val right: Button = Seven
    override val up: Button = Two
    override val down: Button = A
  }

  case object Seven extends Button {
    override val digit: String = "7"
    override val left: Button = Six
    override val right: Button = Eight
    override val up: Button = Three
    override val down: Button = B
  }

  case object Eight extends Button {
    override val digit: String = "8"
    override val left: Button = Seven
    override val right: Button = Nine
    override val up: Button = Four
    override val down: Button = C
  }

  case object Nine extends Button {
    override val digit: String = "9"
    override val left: Button = Eight
    override val right: Button = Nine
    override val up: Button = Nine
    override val down: Button = Nine
  }

  case object A extends Button {
    override val digit: String = "A"
    override val left: Button = A
    override val right: Button = B
    override val up: Button = Six
    override val down: Button = A
  }

  case object B extends Button {
    override val digit: String = "B"
    override val left: Button = A
    override val right: Button = C
    override val up: Button = Seven
    override val down: Button = D
  }

  case object C extends Button {
    override val digit: String = "C"
    override val left: Button = B
    override val right: Button = C
    override val up: Button = Eight
    override val down: Button = C
  }

  case object D extends Button {
    override val digit: String = "D"
    override val left: Button = D
    override val right: Button = D
    override val up: Button = B
    override val down: Button = D
  }

}

class DayTwo extends Puzzle("http://adventofcode.com/2016/day/2/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayTwo.splitInput(input).forall(_.forall(c => c == 'U' || c == 'R' || c == 'L' || c == 'D')))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val directions = DayTwo.splitInput(input)
    DayTwo.followDirections(directions, KeypadOne.Five.asInstanceOf[Button])
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val directions = DayTwo.splitInput(input)
    DayTwo.followDirections(directions, KeypadTwo.Five.asInstanceOf[Button])
  }
}

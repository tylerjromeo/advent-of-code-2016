package org.romeo.adventofcode.day21

import org.romeo.adventofcode.common.Puzzle

/**
  * Created by tylerromeo on 3/3/17.
  */
object DayTwentyOne {

  val srcString = "abcdefgh" //This is my particular input. Not sure if it's the same for everyone. They don't have it in the webservice input.
  val srcString2 = "fbgdceah" //This is my particular input. Not sure if it's the same for everyone. They don't have it in the webservice input.
  def main(args: Array[String]): Unit = {
    new DayTwentyOne().run()
  }

  def parseAll(s: String): Seq[Command] = {
    s.split("\n").map(parse)
  }

  def parse(s: String): Command = {
    val split = s.split(" ")
    if (split(0) == "swap") {
      if (split(1) == "position") {
        SwapPosition(split(2).toInt, split(5).toInt)
      } else if (split(1) == "letter") {
        SwapLetter(split(2).head, split(5).head)
      } else {
        DoNothing()
      }
    } else if(split(0) == "rotate") {
      if(split(1) == "left") {
        RotateLeft(split(2).toInt)
      } else if(split(1) == "right") {
        RotateRight(split(2).toInt)
      } else {
        RotatePosition(split(6).head)
      }
    } else if(split(0) == "reverse") {
      ReversePosition(split(2).toInt, split(4).toInt)
    } else if(split(0) == "move"){
      MovePosition(split(2).toInt, split(5).toInt)
    } else {
      DoNothing()
    }
  }

  def scramble(src: String, commands: Seq[Command]): String = {
    commands.foldLeft(src){ (s, c) =>
      c.run(s)
    }
  }

  def descramble(src: String, commands: Seq[Command]): String = {
    commands.reverse.foldLeft(src){ (s, c) =>
      c.reverse(s)
    }
  }
}

sealed trait Command {
  def run(s: String): String
  def reverse(s: String): String
}

case class SwapPosition(x: Int, y: Int) extends Command {
  override def run(s: String): String = {
    val xChar = s.charAt(x)
    val yChar = s.charAt(y)
    s.patch(x, yChar.toString, 1).patch(y, xChar.toString, 1)
  }

  override def reverse(s: String):String = run(s)
}

case class SwapLetter(x: Char, y: Char) extends Command {
  override def run(s: String): String = {
    // assuming > and < arent in the source data
    val tmp = s.replace(x, '>').replace(y, '<')
    tmp.replace('>', y).replace('<', x)
  }

  override def reverse(s: String):String = run(s)
}

case class RotateLeft(steps: Int) extends Command {
  override def run(s: String): String = {
    val nRotations = steps % s.length
    s.drop(nRotations) ++ s.take(nRotations)
  }

  override def reverse(s: String):String = RotateRight(steps).run(s)
}

case class RotateRight(steps: Int) extends Command {
  override def run(s: String): String = {
    val nRotations = steps % s.length
    s.takeRight(nRotations) ++ s.dropRight(nRotations)
  }

  override def reverse(s: String):String = RotateLeft(steps).run(s)
}

case class RotatePosition(position: Char) extends Command {
  override def run(s: String): String = {
    val charPosition = s.indexOf(position)
    val extraRotation = if(charPosition >= 4) 1 else 0
    val nRotations = 1 + charPosition + extraRotation
    RotateRight(nRotations).run(s)
  }

  override def reverse(s: String): String = {
    val charPosition = s.indexOf(position)
    //cheating a bit here, but since we know the input is 8 characters, we can just do each of the 8 cases
    charPosition match {
      case 0 => RotateLeft(1).run(s)
      case 1 => RotateLeft(1).run(s)
      case 2 => RotateLeft(6).run(s)
      case 3 => RotateLeft(2).run(s)
      case 4 => RotateLeft(7).run(s)
      case 5 => RotateLeft(3).run(s)
      case 6 => RotateLeft(0).run(s)
      case 7 => RotateLeft(4).run(s)
    }
  }
}

case class ReversePosition(x: Int, y: Int) extends Command {
  override def run(s: String): String = {
    s.take(x) ++ s.substring(x,y+1).reverse ++ s.drop(y+1)
  }
  override def reverse(s: String):String = run(s)
}

case class MovePosition(x: Int, y: Int) extends Command {
  override def run(s: String): String = {
    s.patch(x, Nil, 1).patch(y, s.charAt(x).toString, 0)
  }
  override def reverse(s: String):String = MovePosition(y,x).run(s)
}

case class DoNothing() extends Command {
  override def run(s: String): String = {
    s
  }
  override def reverse(s: String):String = run(s)
}


class DayTwentyOne extends Puzzle("http://adventofcode.com/2016/day/21/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayTwentyOne.parseAll(input).forall { command =>
      command.isInstanceOf[Command] && !command.isInstanceOf[DoNothing]
    })
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val commands = DayTwentyOne.parseAll(input)
    DayTwentyOne.scramble(DayTwentyOne.srcString, commands)
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val commands = DayTwentyOne.parseAll(input)
    DayTwentyOne.descramble(DayTwentyOne.srcString2, commands)
  }
}

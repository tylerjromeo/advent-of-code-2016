package org.romeo.adventofcode.day17

import java.security.MessageDigest

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/19/16
  * Time: 9:13 PM
  *
  */
object DaySeventeen {
  def main(args: Array[String]): Unit = {
    new DaySeventeen().run()
  }

  val md5 = MessageDigest.getInstance("MD5")

  def hash(src: String): String = {
    md5.digest(src.getBytes).map("%02X" format _).mkString.toLowerCase
  }

  def isUnlocked(c: Char): Boolean = {
    List('b', 'c', 'd', 'e', 'f').contains(c)
  }
}

// this maze assumes 0 0 is the upper left corner and the solution is the lower right
case class Maze(size: (Int, Int), passcode: String) {
  def possibleMoves(path: Path): Set[Move] = {
    val hash = DaySeventeen.hash(passcode + path.moves.map(_.letter).mkString)
    Set[Option[Move]](
      if (DaySeventeen.isUnlocked(hash(0)) && path.coord._1 != 0) Some(Up) else None,
      if (DaySeventeen.isUnlocked(hash(1)) && path.coord._1 < size._1 - 1) Some(Down) else None,
      if (DaySeventeen.isUnlocked(hash(2)) && path.coord._2 != 0) Some(Left) else None,
      if (DaySeventeen.isUnlocked(hash(3)) && path.coord._2 < size._2 - 1) Some(Right) else None
    ).flatten
  }

  def isSolved(coord: (Int, Int)): Boolean = {
    coord._1 == size._1 - 1 && coord._2 == size._2 - 1
  }
}

sealed trait Move {
  val letter: Char

  def move(from: (Int, Int)): (Int, Int)
}

case object Up extends Move {
  override val letter: Char = 'U'

  override def move(from: (Int, Int)): (Int, Int) = (from._1 - 1, from._2)
}

case object Down extends Move {
  override val letter: Char = 'D'

  override def move(from: (Int, Int)): (Int, Int) = (from._1 + 1, from._2)
}

case object Left extends Move {
  override val letter: Char = 'L'

  override def move(from: (Int, Int)): (Int, Int) = (from._1, from._2 - 1)
}

case object Right extends Move {
  override val letter: Char = 'R'

  override def move(from: (Int, Int)): (Int, Int) = (from._1, from._2 + 1)
}

case class Path(moves: List[Move], coord: (Int, Int)) {
  def nextPath(move: Move): Path = {
    Path(moves ::: List(move), move.move(coord))
  }
}

class DaySeventeen extends Puzzle("http://adventofcode.com/2016/day/17/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    //Do nothing
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val maze = Maze((4, 4), input.trim)
    val paths = scala.collection.mutable.Queue(Path(List(), (0,0)))
    while (!maze.isSolved(paths.head.coord)) {
      val p = paths.dequeue()
      val nextMoves = maze.possibleMoves(p)
      nextMoves.map(p.nextPath).foreach(paths.enqueue(_))
    }
    paths.head.moves.map(_.letter).mkString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val maze = Maze((4, 4), input.trim)
    val paths = scala.collection.mutable.Stack(Path(List(), (0, 0)))
    var longestPath = Path(List(), (0, 0))
    while (paths.nonEmpty) {
      val p = paths.pop()
      if (maze.isSolved(p.coord)) {
        if (p.moves.length > longestPath.moves.length) {
          longestPath = p
        }
      } else {
        val nextMoves = maze.possibleMoves(p)
        paths.pushAll(nextMoves.map(p.nextPath))
      }
    }
    longestPath.moves.length.toString
  }
}

package org.romeo.adventofcode.day9

import org.romeo.adventofcode.common.Puzzle

import scala.annotation.tailrec

/**
  * User: tylerromeo
  * Date: 12/11/16
  * Time: 9:11 AM
  *
  */
object DayNine {
  def main(args: Array[String]): Unit = {
    new DayNine().run()
  }

  def decompress(src: String): String = {
    @tailrec
    def iter(acc:String, src: String): String = {
      if(src.isEmpty) {
        acc
      } else {
        var newSrc: String = null
        var newAcc: String = null
        if(!src.startsWith("(")){
          newAcc = acc + src.head
          newSrc = src.tail
        } else {
          val repeatInstruction = src.tail.takeWhile(_ != ')').split("x")
          val (nChars, nRepeats) = (repeatInstruction(0).toInt, repeatInstruction(1).toInt)
          val stringToRepeat = src.dropWhile(_ != ')').tail.take(nChars)
          newAcc = acc + List.fill(nRepeats)(stringToRepeat).mkString
          newSrc = src.dropWhile(_ !=')').tail.drop(nChars)
        }
        iter(newAcc, newSrc)
      }
    }
    iter("", src)
  }
}
class DayNine extends Puzzle("http://adventofcode.com/2016/day/9/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    //do nothing
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
override def solvePart1(input: String): String = {
  // Tests
//  List("ADVENT", "A(1x5)BC", "(3x3)XYZ", "A(2x2)BCD(2x2)EFG", "(6x1)(1x3)A", "X(8x2)(3x3)ABCY").map(s => {
//    s"$s becomes: ${DayNine.decompress(s)}"
//  }).mkString("\n")
  val noWhitespace = input.replaceAll("\\s+", "")
  DayNine.decompress(noWhitespace).size.toString
}

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

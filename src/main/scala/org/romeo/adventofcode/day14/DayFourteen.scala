package org.romeo.adventofcode.day14

import java.security.MessageDigest

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/14/16
  * Time: 9:18 PM
  *
  */
object DayFourteen {
  def main(args: Array[String]): Unit = {
    new DayFourteen().run()
  }

  val md5 = MessageDigest.getInstance("MD5")

  def hash(src: String): String = {
    md5.digest(src.getBytes).map("%02X" format _).mkString.toLowerCase
  }

  // returns a function that tests if a string contains ANY character i times in a row
  def findRepeat(i: Int): String => Option[Char] = s => {
//    val (_, _, characterCount, consecutiveCharacter) = s.foldLeft(('\0', 1, 0, '\0')){
//      case ((lastChar, count, max, maxChar), nextChar) => {
//        if(nextChar == lastChar) {
//          val newCount = count + 1
//          if(newCount > max) (lastChar, newCount, newCount, lastChar) else (lastChar, newCount, max, maxChar)
//        } else {
//          (nextChar, 1, max, maxChar)
//        }
//      }
//    }
//    if(characterCount >= i) {
////      println(s, consecutiveCharacter)
//      Some(consecutiveCharacter)
//    } else {
//      None
//    }
//    val characters = characterCount(s).filter(_._2 >= i)
//    s.find(characters.contains)
    s.sliding(i).find(_.distinct.length == 1).map(_.apply(0))
  }

  def characterCount(s: String): Map[Char, Int] = {
    s.foldLeft(Map[Char, Int]()){
      case (m, c) => m + (c -> (m.getOrElse(c, 0) + 1))
    }
  }
}


class DayFourteen extends Puzzle("http://adventofcode.com/2016/day/14/input") {
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
//    val hashes = Stream.from(0).map(i => (i, DayFourteen.hash(input.trim + i.toString)))
//    val possibleKeys = hashes.map{
//      case (i, hash) => (i, hash, DayFourteen.findRepeat(3)(hash))
//    }.filter(_._3.nonEmpty)
//    val keys = possibleKeys.filter{
//      case (i, hash, Some(repeat)) => hashes.slice(i + 1, i + 1001).exists{
//        case (_, h) => DayFourteen.findRepeat(5)(h).contains(repeat)
//      }
//    }
//    keys(63)._1.toString
    ""
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    def superHash(string: String): String = {
      (0 to 2016).foldLeft(string){
        case (s, _) => DayFourteen.hash(s)
      }
    }
    val hashes = Stream.from(0).map(i => (i, superHash(input.trim + i.toString)))
    val possibleKeys = hashes.map{
      case (i, hash) => (i, hash, DayFourteen.findRepeat(3)(hash))
    }.filter(_._3.nonEmpty)
    val keys = possibleKeys.filter{
      case (i, hash, Some(repeat)) => hashes.slice(i + 1, i + 1001).exists{
        case (_, h) => DayFourteen.findRepeat(5)(h).contains(repeat)
      }
    }
    keys(63)._1.toString

  }
}

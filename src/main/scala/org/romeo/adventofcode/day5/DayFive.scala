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

  def takeElementFromRight[A](tuple: (Stream[A], Stream[A])): List[A] = {
    tuple._1.toList ++ tuple._2.take(1)
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
        lazy val matchingHashes = Stream.from(0).map(n => DayFive.hash(input + n))
          .filter(_.startsWith(specialPrefix))

        println("This could take a few minutes, hold on")
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
      .filter(s => s.startsWith(specialPrefix) && s(5).asDigit < 8)

    println("This could take a few minutes, hold on")
    //for all of the matching hashes, build up an accumulated set of which positions we have covered
    DayFive.takeElementFromRight(matchingHashes.scanLeft((Set[Int](), "")) {
      case ((acc, _), newString) => (acc + newString(5).asDigit, newString)
    }
      //ignore the first value, since it just has the initial empty string and set in it
      .tail
      //keep getting hashes until we have at least 1 for each index, put those on the left, put the rest on the right
      //takewhile stops as soon as the condition is filled, and we need that last element, so we need to get it from the right side
      .span {
      case (numbers, s) => !(0 to 7).forall(numbers.contains)
    })
      // remove the set accumulation, and convert to tuples of (index, value)
      .map {
      case (_, s) => {
        val index = s(5).asDigit
        (index, s(6))
      }
    }
      // remove all but the first value encountered for each index
      .foldLeft(List[(Int, Char)]()) {
      case (acc, (index, value)) => {
        if (acc.exists { case (i, _) => i == index })
          acc
        else
          (index, value) :: acc
      }
    }.sortBy(_._1).map(_._2).mkString
  }
}

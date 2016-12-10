package org.romeo.adventofcode.day6

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/9/16
  * Time: 3:19 PM
  *
  */
object DaySix {
  def main(args: Array[String]): Unit = {
    new DaySix().run()
  }

  def mode[A](l: List[A]): A = {
    l.groupBy(identity).mapValues(_.size).maxBy { case ((_, s)) => s }._1
  }

  /**
    * returns the least common item in a list
    *
    * @param l
    * @tparam A
    * @return
    */
  def antimode[A](l: List[A]): A = {
    l.groupBy(identity).mapValues(_.size).minBy { case ((_, s)) => s }._1
  }
}

class DaySix extends Puzzle("http://adventofcode.com/2016/day/6/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    val split = input.split("\n")
    val nColumns = split.head.length
    assert(split.forall(_.length == nColumns))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val split = input.split("\n")
    val nColumns = split.head.length
    val columns = split.foldLeft(List.fill(nColumns)(List[Char]()))((acc, s) => {
      acc.zip(s).map { case ((l, c)) => c :: l }
    })
    columns.map(DaySix.mode).mkString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = {
    val split = input.split("\n")
    val nColumns = split.head.length
    val columns = split.foldLeft(List.fill(nColumns)(List[Char]()))((acc, s) => {
      acc.zip(s).map { case ((l, c)) => c :: l }
    })
    columns.map(DaySix.antimode).mkString
  }
}

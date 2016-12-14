package org.romeo.adventofcode.day11

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/12/16
  * Time: 8:04 PM
  *
  */
object DayEleven {
  def main(args: Array[String]): Unit = {
    new DayEleven().run()
  }

  def parseBuilding(input: String): Building = {
    val floors = input.trim.split("\n").
      map(s => {
        //drop "the nth floor contains
        val items = s.split(" ").drop(4).mkString(" ")
        if(items == "nothing relevant."){
          Floor(Set())
        } else {
          Floor(items.split(""", and|,|and""").filter(_.trim.nonEmpty).map(s => parseItem(s.trim)).toSet)
        }
      }
      )
    new Building(0, floors.toList)
  }

  def parseItem(s: String): Item = {
    val split = s.split(" ")
    if(split(2).startsWith("generator")) {
      Generator(split(1))
    } else { //must be "microchip"
      Microchip(split(1).takeWhile(_ != '-'))
    }
  }
}

sealed trait Item

case class Generator(element: String) extends Item

case class Microchip(element: String) extends Item

class Building(val currentFloor: Int, val floors: List[Floor]) {
  if (currentFloor > floors.size - 1) {
    throw new IllegalArgumentException
  }

  def generatePossibleMoves(): Set[Building] = {
    val floor = floors(currentFloor)
    val prevFloor = if (currentFloor != 0) Some(floors(currentFloor - 1)) else None
    val nextFloor = if (currentFloor < floors.size - 1) Some(floors(currentFloor + 1)) else None

    val nextFloorMoves = nextFloor.map(
      Building.generateResultsFromMove(floor, _)
        .map(Building.overwrite(currentFloor, floors)) //TODO: algebra
        .map(new Building(currentFloor + 1, _))
        .toSet
    )
    val prevFloorMoves = prevFloor.map(
      Building.generateResultsFromMove(floor, _)
        .map(_.reverse)
        .map(Building.overwrite(currentFloor - 1, floors)) //TODO: algebra
        .map(new Building(currentFloor - 1, _))
        .toSet
    )

    nextFloorMoves.getOrElse(Set()) ++ prevFloorMoves.getOrElse(Set())
  }

  // return true if the building has all of the equipment on the top floor
  def isComplete(): Boolean = {
    floors.dropRight(1).forall(_.contents.isEmpty) &&
      floors.last.contents.nonEmpty &&
      floors.last.isValid()
  }

  override def toString = s"Building($currentFloor, $floors)"


  def canEqual(other: Any): Boolean = other.isInstanceOf[Building]

  override def equals(other: Any): Boolean = other match {
    case that: Building =>
      (that canEqual this) &&
        currentFloor == that.currentFloor &&
        floors == that.floors
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(currentFloor, floors)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Building {


  //given a start index and a list of data, return a function that returns a new list with the given data replacing the data at the index
  // example: overWrite(1, List(1,2,3,4)(List(99, 100) => List(1, 99, 100, 4)
  def overwrite[A](index: Int, oldList: List[A]): List[A] => List[A] = newList => {
    oldList.take(index) ::: newList ::: oldList.drop(index + newList.size)
  }

  //returns a list of lists of floors, where each inner list has two elements (newSource, newDestination)
  def generateResultsFromMove(source: Floor, destination: Floor): List[List[Floor]] = {
    val moveableItems = source.contents.subsets().toList.filter(s => s.nonEmpty && s.size <= 2)
    moveableItems.map(
      items => List(Floor(source.contents -- items), Floor(destination.contents ++ items))
    ).filter(newFloors => newFloors.forall(_.isValid()))
  }
}

case class Floor(contents: Set[Item]) {
  def isValid(): Boolean = {
    contents.filter(_.isInstanceOf[Microchip]).forall {
      case Microchip(element) => contents.forall(!_.isInstanceOf[Generator]) || contents.contains(Generator(element))
    }
  }
}

class DayEleven extends Puzzle("http://adventofcode.com/2016/day/11/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    val building = DayEleven.parseBuilding(input)
    assert(building.currentFloor == 0)
    assert(building.floors.size == 4)
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val initialState = DayEleven.parseBuilding(input)
    var matchFound = false
    var count = 0
    var state = Set(initialState)
    var checkedStates = Set[Building]() //keep track of states we've already iterated on so they don't get hit twice
    while (!matchFound) {
      // TODO: while loop is cheating, I should use a Stream. Come back and update
      count = count + 1
      checkedStates = checkedStates ++ state
      state = state.flatMap(_.generatePossibleMoves()) -- checkedStates
      matchFound = state.exists(_.isComplete())
    }
    count.toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

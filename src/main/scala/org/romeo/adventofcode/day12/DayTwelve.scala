package org.romeo.adventofcode.day12

import org.romeo.adventofcode.common.Puzzle

/**
  * User: tylerromeo
  * Date: 12/14/16
  * Time: 3:52 PM
  *
  */
object DayTwelve {
  def main(args: Array[String]): Unit = {
    new DayTwelve().run()
  }

  def parseInput(input: String): List[Instruction] = {
    input.split("\n").map(line => {
      val split = line.split(" ")
      val instruction = split(0)
      instruction match {
        case "cpy" => {
          val (value, target) = (split(1), split(2))
          if(value.forall(c => c.isDigit || c == '-')) {
            CopyValue(value.toInt, target)
          } else {
            CopyRegister((c: Computer) => c.getValue(value), target)
          }
        }
        case "inc" => Inc(split(1))
        case "dec" => Dec(split(1))
        case "jnz" => {
          val (value, moves) = (split(1), split(2))
          if(value.forall(c => c.isDigit || c == '-')) {
            JumpNonZeroValue(value.toInt, moves.toInt)
          } else {
            JumpNonZeroRegister((c: Computer) => c.getValue(value), moves.toInt)
          }
        }
        case _ => throw new IllegalArgumentException()
      }
    }).toList
  }
}

case class Register(value: Int = 0)

sealed trait Instruction

case class CopyValue(value: Int, target: String) extends Instruction
case class CopyRegister(value: Computer => Int, targetId: String) extends Instruction

case class Inc(targetId: String) extends Instruction

case class Dec(targetId: String) extends Instruction

case class JumpNonZeroValue(checkValue: Int, move: Int) extends Instruction
case class JumpNonZeroRegister(checkValue: Computer => Int, move: Int) extends Instruction

class Computer(val registers: Map[String, Register]) {

  def getValue(registerId: String): Int = {
    registers.get(registerId).map(_.value).getOrElse(0)
  }
}

class DayTwelve extends Puzzle("http://adventofcode.com/2016/day/12/input") {
  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayTwelve.parseInput(input).forall(_.isInstanceOf[Instruction]))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    val instructions = DayTwelve.parseInput(input)
    var computer = new Computer(Map(
      "a" -> Register(),
      "b" -> Register(),
      "c" -> Register(),
      "d" -> Register()
    ))
    var count = 0
    while(count < instructions.size) {
      val i = instructions(count)
      computer = i match {
        case CopyValue(v, t) => new Computer(computer.registers + (t -> Register(v)))
        case CopyRegister(f, t) => new Computer(computer.registers + (t -> Register(f(computer))))
        case Inc(t) => new Computer(computer.registers + (t -> Register(computer.getValue(t) + 1)))
        case Dec(t) => new Computer(computer.registers + (t -> Register(computer.getValue(t) - 1)))
        case JumpNonZeroValue(v, m) => {
          if(v != 0) {
            count = count + m - 1 //minus 1 since we count up by 1 at the end of the loop
          }
          computer
        }
        case JumpNonZeroRegister(f, m) => {
          if(f(computer) != 0) {
            count = count + m - 1 //minus 1 since we count up by 1 at the end of the loop
          }
          computer
        }
      }
      count = count + 1
    }
    computer.registers("a").value.toString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

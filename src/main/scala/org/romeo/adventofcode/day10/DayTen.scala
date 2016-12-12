package org.romeo.adventofcode.day10

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Inbox}
import akka.pattern.ask
import akka.util.Timeout
import org.romeo.adventofcode.common.Puzzle

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * User: tylerromeo
  * Date: 12/11/16
  * Time: 9:56 AM
  *
  */
object DayTen {
  def main(args: Array[String]): Unit = {
    new DayTen().run()
  }

  def generateGiveFunction(lowTarget: String, highTarget: String): (Int, Int) => List[GiveChip] = {
    (chip1, chip2) => {
      val lowChip = if (chip1 < chip2) chip1 else chip2
      val highChip = if (chip1 < chip2) chip2 else chip1
      List(
        GiveChip(lowTarget, lowChip),
        GiveChip(highTarget, highChip)
      )
    }
  }

  def parseInstructions(input: String): List[BotInstruction] = {
    input.split("\n").map(s => {
      if (s.startsWith("value")) {
        val split = s.split(" ")
        GiveChip(split(5), split(1).toInt).asInstanceOf[BotInstruction]
        //      } else if(s.startsWith("bot")) { //we can assume it starts with "bot" if not "value"
      } else {
        val split = s.split(" ")
        SetFunction(split(1), generateGiveFunction(split(6), split(11))).asInstanceOf[BotInstruction]
      }
    }).toList
  }
}

class Bot extends Actor {
  private var chip1: Option[Int] = None
  private var chip2: Option[Int] = None
  private var giveFunc: Option[(Int, Int) => List[GiveChip]] = None

  override def receive: Receive = {
    case GiveChip(_, x) => {
      if (chip1.isEmpty) {
        chip1 = Some(x)
        botNotReady()
      } else if (chip2.isEmpty) {
        chip2 = Some(x)
        if(giveFunc.isDefined){
          onBotReady()
        } else {
          botNotReady()
        }
      } else {
        throw new RuntimeException(s"Bot with id ${self.path.name}, can't hold more than 2 chips")
      }
    }
    case SetFunction(_, f) => {
      giveFunc = Some(f)
      if (chip1.isDefined && chip2.isDefined) {
        onBotReady()
      } else {
        botNotReady()
      }
    }
  }

  def onBotReady(): Unit = {
    giveFunc.foreach(f => f(chip1.get, chip2.get).foreach(send))
    context.parent ! (self.path.name, chip1.get, chip2.get)
    chip1 = None
    chip2 = None
  }

  def botNotReady(): Unit = {
    sender ! None
  }

  def send(instruction: GiveChip) = {
    context.actorSelection(s"../${instruction.id}") ! instruction
  }
}

class Supervisor(lowTarget: Int, highTarget: Int) extends Actor {
  implicit val timeout = Timeout(1.minute)

  private var origin: Option[ActorRef] = None

  override def receive: Receive = {
    case (lowTarget: Int, highTarget: Int, input: String) => {
      origin = Some(sender)
      val instructions = DayTen.parseInstructions(input)
      //create all the actors first
      val createdActors = scala.collection.mutable.Set[String]()
      instructions.foreach {
        case GiveChip(id, _) => {
          if (!createdActors.contains(id)) {
            createdActors += id
            context.actorOf(Props[Bot], id)
          }
        }
        case SetFunction(id, _) => {
          if (!createdActors.contains(id)) {
            createdActors += id
            context.actorOf(Props[Bot], id)
          }
        }
      }
      //then send the instructions and await the results
      val results = Future.sequence(instructions.map {
        case GiveChip(id, i) => context.actorSelection(id) ? GiveChip(id, i)
        case SetFunction(id, f) => context.actorSelection(id) ? SetFunction(id, f)
      })
    }
    case (id:String, chip1:Int, chip2:Int) => {
      if((chip1 == lowTarget && chip2 == highTarget) || (chip2 == lowTarget && chip1 == highTarget)) {
        origin.foreach(_ ! id)
      }
    }
  }
}

trait BotInstruction

case class GiveChip(id: String, chip: Int) extends BotInstruction

case class SetFunction(id: String, f: (Int, Int) => List[GiveChip]) extends BotInstruction


class DayTen extends Puzzle("http://adventofcode.com/2016/day/10/input") {


  /**
    * read the problem's input and crash hard if it's invalid
    *
    * @param input
    */
  override def validateInput(input: String): Unit = {
    assert(DayTen.parseInstructions(input).forall(_.isInstanceOf[BotInstruction]))
  }

  /**
    * solve part 1 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart1(input: String): String = {
    implicit val timeout = Timeout(1.minute)
    val (lowTarget, highTarget) = (17, 61)
    val system = ActorSystem("BotSystem")
    implicit val i = Inbox.create(system)
    val result = system.actorOf(Props(new Supervisor(lowTarget, highTarget)), "supervisor") ? (lowTarget, highTarget, input)
    val resultString = Await.result(result, timeout.duration).toString
    system.terminate()
    resultString
  }

  /**
    * solve part 2 of the day's problem
    *
    * @param input
    * @return
    */
  override def solvePart2(input: String): String = ???
}

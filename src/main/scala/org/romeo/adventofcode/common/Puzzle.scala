package org.romeo.adventofcode.common

import com.typesafe.config.ConfigFactory

/**
  * User: tylerromeo
  * Date: 12/7/16
  * Time: 9:22 PM
  *
  */
abstract class Puzzle(inputUrl: String) {
  val authKey = "authCookie"
  val webReader = new WebInputReader(inputUrl, getConfigOptional(authKey))

  def getConfigOptional(key: String): Option[String] = {
    val config = ConfigFactory.load()
    if(config.hasPath(key)) Some(config.getString(key)) else None
  }

  def run(): Unit = {
    webReader.content match {
      case Right(x) => {
        println(s"Part one's answer is ${solvePart1(x)}")
        println(s"Part two's answer is ${solvePart2(x)}")
      }
      case Left(e) => println(e)
    }
  }

  def solvePart1(input: String): String
  def solvePart2(input: String): String
}

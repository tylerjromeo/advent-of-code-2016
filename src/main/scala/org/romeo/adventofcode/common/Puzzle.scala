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
      case Right(x) => println(solve(x))
      case Left(e) => println(e)
    }
  }

  def solve(input: String): String
}

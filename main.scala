package com.testDaniel

import scala.collection.immutable.ListMap


object main extends App {


  val game = new Cards()
  println("How much players ? ")
  val user = game.userCardList()

  val board = game.boardCards()
  val result: List[Int] = List()

  var res: Map[String, Int] = user zip game.findUserResult(user, board, result) to Map
  res = ListMap(res.toSeq.sortWith(_._2 > _._2): _*)

  println(res)
}


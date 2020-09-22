package com.testDaniel

import java.util.regex.Pattern

class Cards {

  var playCard: Map[String, Int] = Map[String, Int]("A" -> 14, "K" -> 13,
    "Q" -> 12, "J" -> 11, "T" -> 10)

  def findUserResult(cards: List[String], board: String, res: List[Int]): List[Int] = {
    println(cards)
    if (cards.isEmpty) {
      res
    } else {
      val str = convertCardsToStr(cards.head, board)
      val int = convertCardsToInt(cards.head, board)

      var score = findFlashRoyal(int, str)
      if (score == 0) {
        score = findStraightFlush(int, str)
        if (score == 0) {
          score = findFourKind(int, 0)
          if (score == 0) {
            score = findFullHouse(int)
            if (score == 0) {
              score = findFlush(str)
              if (score == 0) {
                score = findStraight(int, int.take(4))
                if (score == 0) {
                  score = findThreeKind(int, 0)
                  if (score == 0) {
                    score = findPair(int, 0)
                    if (score == 3) {
                      score = score - 1
                    }
                  }
                }
              }
            }
          }
        }
      }
      findUserResult(cards.slice(1, cards.length), board, res = res :+ score)
    }

  }

  def boardCards(): String = {

    val cards: String = scala.io.StdIn.readLine()

    if (cards.length != 10 || !inputIsValid(cards)) {
      println("Is invalid, try again, pay Attention to (one of \"A\", \"K\", \"Q\",\n\"J\", \"T\", \"9\", \"8\", \"7\", " +
        "\"6\", \"5\", \"4\", \"3\", \"2\") and the second character representing the suit (one of\n\"h\", \"d\", \"c\", \"s\")")
      boardCards()
    }
    else return cards

  }

  def countSubstring(str: String, substr: String): Int = {
    substr.r.findAllMatchIn(str).length
  }

  def convertCardsToInt(userCard: String, boardCard: String): List[Int] = {
    val result: List[Int] = List()
    transformCardNumbers(userCard + boardCard, result)
  }

  def convertCardsToStr(userCard: String, boardCard: String): List[String] = {
    val result: List[String] = List()
    transformCardSymbols(userCard + boardCard, result)
  }

  def findPair(cards: List[Int], score: Int): Int = {
    val temp = findPairAmount(cards)
    if (temp == 2 && cards.length > 1) {
      findPair(cards.slice(temp, cards.length), score = score + 1)
    } else if (temp > 2 && cards.length > 1) {
      findPair(cards.slice(temp, cards.length), score)
    } else if (temp == 0 && cards.length > 1) {
      findPair(cards.slice(1, cards.length), score)
    } else score
  }

  def findThreeKind(cards: List[Int], score: Int): Int = {
    val temp = findPairAmount(cards)
    if (cards.count(_ == cards.head) == 3 && cards.length > 1) {
      findThreeKind(cards.slice(temp, cards.length), score = score + 3)
    } else if (cards.count(_ == cards.head) != 0 && cards.length > 1 && temp != 0) {
      findThreeKind(cards.slice(temp, cards.length), score)
    } else if (cards.count(_ == cards.head) == 0 && cards.length > 1) {
      findThreeKind(cards.slice(1, cards.length), score)
    }
    else score
  }

  def findPairAmount(cards: List[Int]): Int = {

    if (cards.count(_ == cards.head) == 2) 2
    else if (cards.count(_ == cards.head) == 3) 3
    else if (cards.count(_ == cards.head) == 4) 4

    else 0
  }

  def findStraight(cards: List[Int], ifAce: List[Int]): Int = {
    var userCardList: List[Int] = cards

    if (ifAce.sum <= 14) {

      userCardList = userCardList.slice(0, 6)
      userCardList = userCardList.::(1)
    }
    if (straightAlg(userCardList, 0, 0)) 4
    else if (straightAlg(userCardList, 0, 0)) 4
    else 0
  }

  def findFlush(cards: List[String]): Int = {
    if (cards.count(_ == cards.head) == 5) 5
    else if (cards.length < 4) {
      0
    } else {
      findFlush(cards.slice(1, cards.length))
    }
  }

  def findFullHouse(cards: List[Int]): Int = {
    val pair = findPair(cards, 0)
    val threePair = findThreeKind(cards, 0)
    if ((pair == 2 || pair == 1) && threePair == 3) {
      6
    } else {
      0
    }
  }

  def findFourKind(cards: List[Int], score: Int): Int = {
    val temp = findPairAmount(cards)
    if (temp == 4 && cards.length > 1) {
      findFourKind(cards.slice(temp, cards.length), score = score + 7)
    } else if (temp != 0 && cards.length > 1 && temp != 0) {
      findFourKind(cards.slice(temp, cards.length), score)
    } else if (temp == 0 && cards.length > 1) {
      findFourKind(cards.slice(1, cards.length), score)
    }
    else score
  }

  def findStraightFlush(num: List[Int], str: List[String]): Int = {
    val straight = findStraight(num, num.take(4))
    val flush = findFlush(str)

    if (flush != 0 && straight != 0) 8
    else 0
  }

  def findFlashRoyal(num: List[Int], str: List[String]): Int = {

    val straight = findStraight(num, num.take(4))
    val flush = findFlush(str)

    if (num.sum >= 60 && (flush != 0 && straight != 0)) 100
    else 0
  }

  def findSymbValue(symbol: String): Int = {
    if (playCard.contains(symbol)) {
      playCard(symbol)
    } else if (isNumeric(symbol)) symbol.toInt else 0
  }

  def generateList(sortedList: List[Int]): List[Int] = {
    var result: List[Int] = List()
    if (sortedList.head > 7) {
      for (a <- 7 to 14) {
        result = result :+ a
      }

    } else {
      for (a <- sortedList.head to 14) {
        result = result :+ a
      }
    }
    result
  }

  def isNumeric(str: String): Boolean = scala.util.Try(str.toInt).isSuccess

  def inputIsValid(board: String): Boolean = {
    val pattern = Pattern.compile("[a-zA-Z0-9]*")


    val matcher = pattern.matcher(board)

    if (matcher.matches) return true
    else return false
  }

  def straightAlg(cardList: List[Int], score: Int, step: Int): Boolean = {
    if (score == 4) return true
    if (score != 4 && step == cardList.length - 1) {
      false
    }
    else {
      if (cardList(step + 1) - cardList(step) == 1) {
        straightAlg(cardList, score + 1, step + 1)
      }
      else if (cardList(step + 1) - cardList(step) == 0) {
        straightAlg(cardList, score, step + 1)
      }
      else straightAlg(cardList, 0, step + 1)
    }

  }

  def sliceCards(card: String, start: Int): String = {
    card.slice(start, start + 1)
  }

  def transformCardNumbers(card: String, cardList: List[Int]): List[Int] = {

    if (card.length == 0) {

      cardList.sorted
    } else {

      transformCardNumbers(card.slice(2, card.length), cardList = cardList :+ findSymbValue(card.slice(0, 1)))

    }

  }

  def transformCardSymbols(card: String, cardList: List[String]): List[String] = {

    if (card.length == 0) cardList
    else {

      transformCardSymbols(card.slice(2, card.length), cardList = cardList :+ card.slice(1, 2))

    }

  }

  def userCards(): String = {
    val cards: String = scala.io.StdIn.readLine()

    if (cards.length != 4 || !inputIsValid(cards)) {
      println("Is invalid, try again, pay Attention to (one of \"A\", \"K\", \"Q\",\n\"J\", \"T\", \"9\", \"8\", \"7\", " +
        "\"6\", \"5\", \"4\", \"3\", \"2\") and the second character representing the suit (one of\n\"h\", \"d\", \"c\", \"s\")")
      userCards()
    } else
      cards
  }

  def userCardList(): List[String] = {
    List.fill(scala.io.StdIn.readInt())(userCards())
  }

}


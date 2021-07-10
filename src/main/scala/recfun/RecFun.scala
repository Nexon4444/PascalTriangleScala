package recfun

import scala.::
import scala.annotation.tailrec

object RecFun extends RecFunInterface :

  def main(args: Array[String]): Unit = {
//    println(countChange(4, List(1, 2)) == 3)
//    println(countChange(300,List(5,10,20,50,100,200,500)) == 1022)
//    println(countChange(301,List(5,10,20,50,100,200,500)) == 0)
//    println(countChange(300,List(500,5,50,100,20,200,10)) == 1022)
//    println(!balance(":-)".toList))
//    println(!balance("())(".toList))
//    println(balance("((())())".toList))
//    println(balance("'(if (zero? x) max (/ 1 x))".toList))

    println("Pascal's Triangle")
    println(pascal(1, 3))
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    @tailrec
    def generateRows(rowNum: Int, middle: Seq[Int], rows: Seq[Seq[Int]] = Seq(Seq(1, 1))): Seq[Seq[Int]] = {
      val nextRow = 1 +: middle.sliding(2).toSeq.map(_.sum) :+ 1
      if (rowNum < r) generateRows(rowNum + 1, nextRow, rows :+ nextRow)
      else rows
    }

    val triangle: Seq[Seq[Int]] = generateRows(0, Seq(1, 1))
    triangle(r-1)(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def check(charsLeft: List[Char], count: Int): Boolean = {
      if (count < 0) false
      else if (charsLeft.isEmpty) true
      else if (charsLeft.head == '(') check(charsLeft.tail, count + 1)
      else if (charsLeft.head == ')') check(charsLeft.tail, count - 1)
      else check(charsLeft.tail, count)
    }

    check(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countWithReps(money: Int, used: List[Int] = List.empty): List[List[Int]] = {
      if (money == 0) List(used)
      else if (money<0) List.empty
      else {
        val getLastLargestVal = if (used.isEmpty) (analyzed: Int) => true else (analyzed: Int) => analyzed <= used.last
        coins.filter(getLastLargestVal).flatMap { coin => countWithReps(money - coin, used :+ coin) }
        //      List.empty
      }
    }

    val x: Seq[List[Int]] = countWithReps(money) //.map(_.sorted).distinct // .count
    x.count(_ => true)
  }

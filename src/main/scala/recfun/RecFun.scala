package recfun

import scala.::
import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    @tailrec
    def generateRows(rowNum: Int, middle: Seq[Int], rows: Seq[Seq[Int]] = Seq(Seq.empty)): Seq[Seq[Int]] = {
      val nextRow = 1 +: middle.sliding(2).toSeq.map(_.sum) :+ 1
//      if (rowNum == 0) List(List(1, 1))
      if (rowNum < c) {
        generateRows(rowNum + 1, nextRow, rows :+ nextRow)
      } else {
        rows
      }
    }
    val triangle: Seq[Seq[Int]] = generateRows(0, Seq(1, 1))
    triangle(c)(r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = ???

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???

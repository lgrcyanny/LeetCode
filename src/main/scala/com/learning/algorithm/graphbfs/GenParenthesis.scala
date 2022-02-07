package com.learning.algorithm.graphbfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object GenParenthesis {

  case class Parenthesis(s: String, numLeft: Int, numRight: Int) {
    def isValid(n: Int): Boolean = numLeft <= n && numRight <= n && numLeft >= numRight
    def addLeft(): Parenthesis = Parenthesis(s + "(", numLeft + 1, numRight)
    def addRight(): Parenthesis = Parenthesis(s + ")", numLeft, numRight + 1)
  }

  def generateParenthesis(n: Int): List[String] = {
    val queue = new mutable.Queue[Parenthesis]()
    queue.enqueue(Parenthesis("(", 1, 0))
    val res = new ArrayBuffer[String]()
    while (queue.nonEmpty) {
      val front = queue.dequeue()
      if (front.s.size == 2 * n && front.isValid(n)) {
        res.append(front.s)
      } else {
        val left = front.addLeft()
        if (left.isValid(n)) {
          queue.enqueue(left)
        }
        val right = front.addRight()
        if (right.isValid(n)) {
          queue.enqueue(right)
        }
      }
    }
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val res = generateParenthesis(3)
    println(res.mkString(", "))
  }
}

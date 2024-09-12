package com.learning.algorithm.stack

import scala.collection.mutable
import scala.util.Try

object Calculator {

  def calculate(s: String): Int = {
    val priority = Map(')' -> 0, '-' -> 1, '+' -> 1, '(' -> 2)
    val numStack = new mutable.ArrayStack[Int]()
    val charStack = new mutable.ArrayStack[Char]()
    def _popAndCalc(): Unit = {
      charStack.pop() match {
        case '+' =>
          val right = numStack.pop()
          val left = numStack.pop()
          val res = left + right
          numStack.push(res)
        case '-' =>
          val right = numStack.pop()
          val left = numStack.pop()
          val res = left - right
          numStack.push(res)
        case _ =>
      }
    }
    def _isNumber(c: Char): Boolean = c >= '0' && c <= '9'
    implicit def _charToInt(c: Char): Int = c - '0'
    for (i <- 0 until s.length) {
      val ch = s(i)
      if (ch == ' ') {
        // pass
      } else if (_isNumber(ch)) {
        val lastCh = Try(s(i - 1)).getOrElse(' ')
        if (_isNumber(lastCh)) {
          numStack.push(numStack.pop() * 10 + _charToInt(ch))
        } else {
          numStack.push(_charToInt(ch))
        }
      } else if (ch == '+' || ch == '-') {
        while (!charStack.isEmpty  && priority(ch) <=  priority(charStack.top)) {
          _popAndCalc()
        }
        charStack.push(ch)
      } else if (ch == '(') {
        charStack.push(ch)
      } else if (ch == ')') {
        while (!charStack.isEmpty && charStack.top != '(') {
          _popAndCalc()
        }
        charStack.pop()
      }
    }
    while (!charStack.isEmpty) {
      _popAndCalc()
    }
    numStack.top
  }

  def calculate2(s: String): Int = {
    val ops = new mutable.ArrayStack[Int]()
    ops.push(1)
    var sign = 1
    var res = 0
    var i = 0
    while (i < s.length) {
      s(i) match {
        case ' ' =>
          i = i + 1
        case '+' =>
          sign = ops.top
          i = i + 1
        case '-' =>
          sign = -ops.top
          i = i + 1
        case '(' =>
          ops.push(sign)
          i = i + 1
        case ')' =>
          ops.pop()
          i = i + 1
        case _ =>
          var num = 0
          while (i < s.length && s(i) >= '0' && s(i) <= '9') {
            num = num * 10 + s(i) - '0'
            i = i + 1
          }
          res = res + sign * num
      }
    }
    res
  }



  def main(args: Array[String]): Unit = {
    val s = "1 - (-2)"
    println(calculate2(s))
    // bad case: sign number, 1 - (-2)
  }

}

package com.learning.algorithm.stack

import scala.collection.mutable

object ParenthesisMatch {

  def isValid(s: String): Boolean = {
    if (s.isEmpty) {
      true
    } else {
      val stack = new mutable.ArrayStack[Char]()
      var isMatch = true
      for (c <- s if isMatch) {
        c match {
          case '(' | '{' | '[' =>
            stack.push(c)
          case ')'  =>
            if (stack.size == 0 || stack.top != '(') {
              isMatch = false
            } else if (stack.top == '(') {
              stack.pop()
            }
          case '}'  =>
            if (stack.size == 0 || stack.top != '{') {
              isMatch = false
            } else if (stack.top == '{') {
              stack.pop()
            }
          case ']'  =>
            if (stack.size == 0 || stack.top != '[') {
              isMatch = false
            } else if (stack.top == '[') {
              stack.pop()
            }
          case _ =>
        }
      }
      isMatch && stack.size == 0
    }
  }

  def main(args: Array[String]): Unit = {
    val s = "()"
    println(isValid(s))
  }

}

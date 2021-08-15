package com.learning.leetcode

/**
  * Created by lgrcyanny on 16/11/3.
  */
object HelloWorld {

  implicit class ToString(a: Int) {
    def asString(): String = a.toString
  }

  def main(args: Array[String]): Unit = {
    val a = 7
    println(a.asString())
  }

}

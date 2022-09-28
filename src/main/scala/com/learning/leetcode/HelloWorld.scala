package com.learning.leetcode

/**
  * Created by lgrcyanny on 16/11/3.
  */
object HelloWorld {

  implicit class ToString(a: Int) {
    def asString(): String = a.toString
  }

  def main(args: Array[String]): Unit = {
    val data = Array(1, 2, 3)
    println(data.mkString(", "))
  }

}

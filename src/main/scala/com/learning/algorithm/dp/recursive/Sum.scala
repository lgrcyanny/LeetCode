package com.learning.algorithm.dp.recursive

object Sum {

  def doSum(n: Int): Int = if (n <= 1) n else n + doSum(n - 1)

  def main(args: Array[String]): Unit = {
    println(doSum(1000))
  }

}

package com.learning.algorithm.probability

object MonteCarlo {

  /**
    * User monte carlo calculate Pi
    */
  def process(n: Long): Double = {
    var m: Long = 0
    var i: Long = 0
    while (i < n) {
      val x = Math.random()
      val y = Math.random()
      if (x * x + y * y <= 1) {
        m = m + 1
      }
      i = i + 1
    }
    val pi = m * 1.0 / n * 4
    pi
  }

  def main(args: Array[String]): Unit = {
    println(s"Pi is ${process(100000000L)}")
  }

}

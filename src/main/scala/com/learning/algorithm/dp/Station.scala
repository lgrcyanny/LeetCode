package com.learning.algorithm.dp

object Station {

  val cost = Array(
    Array(0, 10, 75, 94),
    Array(-1, 0, 35, 50),
    Array(-1, -1, 0, 80),
    Array(-1, -1, -1, 0)
  )

  def minCost(s: Int, d: Int): Int = {
    if (s == d || s + 1 == d) {
      cost(s)(d)
    } else {
      var minValue: Int = cost(s)(d)
      for (i <- s + 1 until d) {
        val tmp = minCost(s, i) + minCost(i, d)
        if (tmp < minValue) {
          minValue = tmp
        }
      }
      minValue
    }
  }

  def main(args: Array[String]): Unit = {
    println(minCost(0, 1))
  }

}

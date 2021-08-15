package com.learning.algorithm.dp

object Station {

  /**
   * calc min cost from station s to station d
   */
  val cost = Array(
    Array(0, 10, 75, 94),
    Array(-1, 0, 35, 50),
    Array(-1, -1, 0, 80),
    Array(-1, -1, -1, 0)
  )

  def minCost(s: Int, d: Int): Int = {
    val memo = Array.ofDim[Int](4, 4)
    def iter(s: Int, d: Int): Int = {
      if (memo(s)(d) == 0) {
        if (s == d || s + 1 == d) {
          cost(s)(d)
        } else {
          var minValue: Int = cost(s)(d)
          for (i <- s + 1 until d) {
            val tmp = iter(s, i) + iter(i, d)
            if (tmp < minValue) {
              minValue = tmp
            }
          }
          memo(s)(d) = minValue
          minValue
        }
      } else {
        memo(s)(d)
      }
    }
    iter(s, d)
  }

  def minCostInDP(d: Int): Int = {
    val n = cost.length
    val minCost = new Array[Int](n)
    minCost(1) = cost(0)(1)
    for (i <- 2 until n) {
      minCost(i) = cost(0)(i)
      for (j <- 1 until i) {
        if (minCost(i) > minCost(j) + cost(i)(j)) {
          minCost(i) = minCost(j) + cost(i)(j)
        }
      }
    }
    minCost(d)
  }

  def main(args: Array[String]): Unit = {
    println(minCost(0, 3))
    println(minCostInDP(3))
  }

}

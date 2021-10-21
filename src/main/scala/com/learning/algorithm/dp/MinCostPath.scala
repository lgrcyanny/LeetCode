package com.learning.algorithm.dp

object MinCostPath {
  /**
   * calculate min cost from top left to bottom right
   */
  val cost: Array[Array[Int]] = Array(
    Array(1, 3, 5, 8),
    Array(4, 2, 1, 7),
    Array(4, 3, 2, 3)
  )

  /**
   * O(2 ^ n)
   */
  def minCostPathRecur(cost: Array[Array[Int]], m: Int, n: Int): Int = {
    if (m == 0 && n == 0) {
      cost(0)(0)
    } else if (m == 0) {
      minCostPathRecur(cost, m, n - 1) + cost(0)(n)
    } else if (n == 0) {
      minCostPathRecur(cost, m - 1, n) + cost(m)(0)
    } else {
      Math.min(minCostPathRecur(cost, m - 1, n), minCostPathRecur(cost, m, n - 1)) + cost(m)(n)
    }
  }

  /**
   * @param cost
   * @param m m+1 is cost array row length
   * @param n n+1 is cost array column
   * @return min cost length
   */
  def minCostPathDP(cost: Array[Array[Int]], m: Int, n: Int): Int = {
    val memo = Array.ofDim[Int](m + 1, n + 1)
    memo(0)(0) = cost(0)(0)
    for (i <- 1 to m) {
      memo(i)(0) = memo(i - 1)(0) + cost(i)(0)
    }
    for (j <- 1 to n) {
      memo(0)(j) = memo(0)(j - 1) + cost(0)(j)
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        memo(i)(j) = Math.min(memo(i - 1)(j), memo(i)(j - 1)) + cost(i)(j)
      }
    }
    memo(m)(n)
  }

  def main(args: Array[String]): Unit = {
    val m = cost.length - 1
    val n = cost(0).length - 1
    println(m, n)
    println(minCostPathRecur(cost, m, n))
    println(minCostPathDP(cost, m, n))
  }

}

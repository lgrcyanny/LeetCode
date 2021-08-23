package com.learning.algorithm.dp

object TotalPathCount {

  /**
   * exponential
   */
  def getCount(m: Int, n: Int): Int = {
    if (m == 0 && n == 0) {
      0
    } else if (m == 0 || n == 0) {
      1
    } else {
      getCount(m - 1, n) + getCount(m, n - 1)
    }
  }

  /**
   * O(n ^ 2)
   */
  def getCountDP(m: Int, n: Int): Int = {
    val memo = Array.ofDim[Int](m + 1, n + 1)
    memo(0)(0) = 0
    for (i <- 1 to m) {
      memo(i)(0) = 1
    }
    for (j <- 1 to n) {
      memo(0)(j) = 1
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        memo(i)(j) = memo(i - 1)(j) + memo(i)(j - 1)
      }
    }
    memo(m)(n)
  }

  def main(args: Array[String]): Unit = {
    println(getCount(2, 3))
    println(getCountDP(2, 3))
  }

}

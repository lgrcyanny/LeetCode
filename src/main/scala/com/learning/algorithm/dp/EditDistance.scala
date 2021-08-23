package com.learning.algorithm.dp

object EditDistance {

  /**
   * forward
   */
  def calcEditDistanceV1(s: String, d: String, i: Int = 0, j: Int = 0): Int = {
    if (s == null || i >= s.size) {
      d.size - j
    } else if (d == null || j >= d.size) {
      s.size - i
    } else if (s(i) == d(j)) {
      calcEditDistanceV1(s, d, i + 1, j + 1)
    } else {
      val deleteCost = calcEditDistanceV1(s, d, i + 1, j)
      val insertCost = calcEditDistanceV1(s, d, i, j + 1)
      val updateCost = calcEditDistanceV1(s, d, i + 1, j + 1)
      Array(deleteCost, insertCost, updateCost).min + 1
    }
  }

  /**
   * backward
   */
  def calcEditDistanceV2(s: String, d: String): Int = {
    def recur(m: Int, n: Int): Int = {
      if (m == 0) {
        n
      } else if (n == 0) {
        m
      } else if (s(m - 1) == d(n - 1)) {
        recur(m - 1, n - 1)
      } else {
        val deleteCost = recur(m - 1, n)
        val insertCost = recur(m, n - 1)
        val updateCost = recur(m - 1, n - 1)
        Array(deleteCost, insertCost, updateCost).min + 1
      }
    }
    recur(s.size, d.size)
  }

  def calcEditDistanceDP(s: String, d: String): Int = {
    val m = s.size
    val n = d.size
    val memo: Array[Array[Int]] = Array.ofDim[Int](m + 1, n + 1)
    for (i <- 0 to m) {
      memo(i)(0) = i
    }
    for (j <- 0 to n) {
      memo(0)(j) = j
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s(i - 1) == d(j - 1)) {
          memo(i)(j) = memo(i - 1)(j - 1)
        } else {
          // delete, insert, update
          memo(i)(j) = Array(memo(i - 1)(j), memo(i)(j - 1), memo(i - 1)(j - 1)).min + 1
        }
      }
    }
    memo(m - 1)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val s = "a cat"
    val d = "the cats"
    println(s"v1 edit distance: ${calcEditDistanceV1(s, d)}")
    println(s"v2 edit distance: ${calcEditDistanceV2(s, d)}")
    println(s"edit distance dp: ${calcEditDistanceDP(s, d)}")
  }

}

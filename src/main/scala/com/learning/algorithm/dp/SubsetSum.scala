package com.learning.algorithm.dp

import scala.collection.mutable.ArrayBuffer

object SubsetSum {

  def verifySubsetSum(xs: Array[Int], x: Int): Boolean = {
    def recur(visited: Set[Int], x: Int): Boolean = {
      if (visited.size <= xs.size && x == 0) {
        true
      } else if (x < 0) {
        false
      } else if (visited.size == xs.size) {
        false
      } else {
        val result = new ArrayBuffer[Boolean]
        for (i <- 0 until xs.size) {
          if (!visited.contains(i) && result.find(_ == true).isEmpty) {
            result.append(recur(visited + i, x - xs(i)))
          }
        }
        result.find(_ == true).isDefined
      }
    }
    recur(Set(), x)
  }

  /**
   * O(2^n)
   */
  def verifySubsetSumV2(xs: Array[Int], x: Int): Boolean = {
    def recur(index: Int, n: Int, x: Int): Boolean = {
      if (x == 0) {
        true
      } else if (n == 0) {
        false
      } else if (xs(index) > x) {
        recur(index + 1, n - 1, x)
      } else {
        recur(index + 1, n - 1, x - xs(index)) || recur(index + 1, n - 1, x)
      }
    }
    recur(0, xs.size, x)
  }

  def verifySubsetSumDP(xs: Array[Int], x: Int): Boolean = {
    // memo[i][j] is true if xs[0..i - 1] has subset sum equal to j
    val m = xs.size + 1
    val n = x + 1
    val memo = Array.ofDim[Boolean](m, n)
    memo(0)(0) = true // empty subset for zero value
    for (i <- 1 until m) {
      memo(i)(0) = true // empty subset for zero value
    }
    for (j <- 1 until n) {
      memo(0)(j) = false
    }
    for (i <- 1 until m) {
      for (j <- 1 until n) {
        // memo(i)(j) is true because the cell just above is true
        memo(i)(j) = memo(i - 1)(j)
        if (j >= xs(i - 1)) {
          memo(i)(j) = memo(i)(j) || memo(i - 1)(j - xs(i - 1))
        }
      }
    }
    // print memo:
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        print(memo(i)(j) + ", ")
      }
      println()
    }
    memo(m - 1)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val input = Array(3, 2, 7, 1)
    val X = 8
    println(verifySubsetSum(input, X))
    println(verifySubsetSumV2(input, X))
    println(verifySubsetSumDP(input, X))
  }

}

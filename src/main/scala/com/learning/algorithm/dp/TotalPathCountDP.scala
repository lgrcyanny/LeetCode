package com.learning.algorithm.dp

object TotalPathCountDP {

  def getCount(m: Int, n: Int): Int = {
    val memo = Array.ofDim[Int](m, n)
    memo(0)(0) = 0
    for (i <- 1 until m) {
      memo(i)(0) = 1
    }
    for (j <- 1 until n) {
      memo(0)(j) = 1
    }
    for (i <- 1 until m) {
      for (j <- 1 until n) {
        memo(i)(j) = memo(i - 1)(j) + memo(i)(j - 1)
      }
    }
    memo(m - 1)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    println(getCount(3, 4))
  }

}

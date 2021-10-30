package com.learning.algorithm.dp2.linear.lis

object MaxSubMatrix {

  def getMaxMatrix(matrix: Array[Array[Int]]): Array[Int] = {
    val res = Array.ofDim[Int](4)
    val N = matrix.length
    val M = matrix(0).length
    var maxSum = matrix(0)(0)
    var maxEnding = 0
    var tempX = 0 // temp coordinate
    var tempY = 0 // temp coordinate
    val colSum = Array.ofDim[Int](M) // column sum from i to j

    for (i <- 0 until N) {
      for (t <- 0 until M) {
        colSum(t) = 0
      }
      for (j <- i until N) {
        maxEnding = 0 // dp[i - 1]
        for (k <- 0 until M) {
          colSum(k) = colSum(k) + matrix(j)(k)
          if (maxEnding > 0) {
            maxEnding = colSum(k) + maxEnding
          } else {
            maxEnding = colSum(k)
            tempX = i
            tempY = k
          }
          if (maxEnding > maxSum) {
            maxSum = maxEnding
            res(0) = tempX
            res(1) = tempY
            res(2) = j
            res(3) = k
          }
        }
      }
    }
    println(s"max sum is ${maxSum}")
    res
  }

  def main(args: Array[String]): Unit = {
    val matrix = Array(
      Array(-1, -2, -9, 6),
      Array(8, -9, -3, -6),
      Array(2, 9, -7, -6))
    println(getMaxMatrix(matrix).mkString(", "))
  }

}

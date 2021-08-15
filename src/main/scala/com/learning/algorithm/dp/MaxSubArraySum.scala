package com.learning.algorithm.dp

object MaxSubArraySum {

  /**
   * O(n ^ 2)
   */
  def search(arr: Array[Int]): (Array[Int], Int) = {
    var subArray = arr
    var maxSum = arr.max
    var tmpSum = 0
    for (i <- 0 until arr.length) {
      tmpSum = arr(i)
      for (j <- i + 1 until arr.length) {
        tmpSum = tmpSum + arr(j)
        if (tmpSum > maxSum) {
          maxSum = tmpSum
          subArray = arr.slice(i, j + 1)
        }
      }
    }
    (subArray, maxSum)
  }

  /**
   * O(n): Jay Kadane algorithm
   */
  def searchDP(arr: Array[Int]): Int = {
    var maxSumSoFar = 0
    var maxSumEndingHere = 0
    for (i <- 0 until arr.length) {
      maxSumEndingHere = maxSumEndingHere + arr(i)
      if (maxSumEndingHere < 0) {
        maxSumEndingHere = 0
      }
      if (maxSumSoFar < maxSumEndingHere) {
        maxSumSoFar = maxSumEndingHere
      }
    }
    maxSumSoFar
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(-2, -3, 4, -1, -2, 1, 5, -3)
    val (subArray, maxSum) = search(arr)
    println(s"result ${maxSum}, array: ${subArray.mkString(",")}")
    println(s"result by dp is ${searchDP(arr)}")
  }

}

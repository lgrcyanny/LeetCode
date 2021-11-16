package com.learning.algorithm.dp2.linear.single.fib

import scala.collection.mutable

object LongestFibSubSeq {

  def longestFibSubSeq(nums: Array[Int]): Int = {
    val n = nums.size
    val index = new mutable.HashMap[Int, Int]()
    for (i <- 0 until n) {
      index.put(nums(i), i)
    }
    var res = 0
    // dp as HashMap because 2-dimension array is sparse
    val dp = new mutable.HashMap[Int, Int]()
    for (k <- 0 until n) {
      for (j <- 0 until k) {
        // test if A[k] = A[i] + A[j]
        val i = index.getOrElse(nums(k) - nums(j), -1)
        if (i >= 0 && i < j) {
          dp(j * n + k) = dp.getOrElse(i * n + j, 2) + 1
          res = Math.max(res, dp(j * n + k))
        }
      }
    }
    if (res >= 3) {
      res
    } else {
      0
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2, 3, 4, 5, 6, 7, 8) // expect 5
    println(longestFibSubSeq(nums))
  }

}

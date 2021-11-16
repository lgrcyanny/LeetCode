package com.learning.algorithm.dp2.linear.single.fib

import scala.collection.mutable

object LongestArithSeq {
  def longestArithSeqLen(nums: Array[Int]): Int = {
    val n = nums.size
    if (n < 2) {
      0
    } else {
      var res = 2
      val dp = new mutable.HashMap[Int, mutable.HashMap[Int, Int]]()
      for (i <- 0 until n) {
        dp(i) = new mutable.HashMap[Int, Int]()
      }
      for (i <- 1 until n) {
        for (j <- (i - 1) to 0 by -1) {
          val diff = nums(i) - nums(j)
          val len = Math.max(dp(j).getOrElse(diff, 1) + 1, dp(i).getOrElse(diff, 1))
          dp(i).put(diff, len)
          res = Math.max(res, len)
        }
      }
      res
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(44,46,22,68,45,66,43,9,37,30,50,67,32,47,44,11,15,4,11,6,20,64,54,54,61,63,23,43,3,12,51,61,16,57,14,12,55,17,18,25,19,28,45,56,29,39,52,8,1,21,17,21,23,70,51,61,21,52,25,28)
    println("res " + longestArithSeqLen(nums))
  }
}

package com.learning.algorithm.dp2.linear.single.rob

object Rob {

  def robRecur(nums: Array[Int]): Int = {
    def _recur(i: Int): Int = {
      if (i == 0) {
        nums(0)
      } else {
        var maxSum = nums(i)
        for (j <- 0 until i - 1) {
          val t = Math.max(nums(i), _recur(j) + nums(i))
          maxSum = Math.max(t, maxSum)
        }
        maxSum
      }
    }
    val res = for (i <- 0 until nums.size) yield _recur(i)
    println(s"${res.mkString(", ")}")
    res.max
  }

  def rob(nums: Array[Int]): Int = {
    val n = nums.size
    var maxSum = Int.MinValue
    val memo = Array.ofDim[Int](n)
    memo(0) = nums(0)
    for (i <- 0 until n) {
      memo(i) = nums(i)
      for (j <- 0 until i - 1) {
        memo(i) = Math.max(memo(i), Math.max(nums(i), nums(i) + memo(j)))
      }
      maxSum = Math.max(maxSum, memo(i))
    }
    println(s"${memo.mkString(", ")}")
    maxSum
  }

  def robOpt(nums: Array[Int]): Int = {
    val n = nums.size
    if (n == 1) {
      nums(0)
    } else {
      var maxEnding0 = nums(0)
      var maxEnding1 = Math.max(nums(1), maxEnding0)
      var maxSum = maxEnding1
      for (i <- 2 until n) {
        val t = Math.max(maxEnding1, maxEnding0 + nums(i))
        maxEnding0 = maxEnding1
        maxEnding1 = t
        maxSum = Math.max(maxSum, t)
      }
      maxSum
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(2, 1, 1, 2)
    println(robRecur(nums))
    println(rob(nums))
    println(robOpt(nums))
  }

}

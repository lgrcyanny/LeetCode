package com.learning.algorithm.dp2.linear.single.maxsub

object MaxSubArr {

  def getMaxSubArr(nums: Array[Int]): Int = {
    def recur(i: Int): Int = {
      if (i == -1) {
        0
      } else {
        Math.max(recur(i - 1), 0) + nums(i)
      }
    }

    val maxValue = (for (i <- 0 until nums.size) yield recur(i)).max
    maxValue
  }

  def getMaxSubArrDP(nums: Array[Int]): Int = {
    var maxSum = nums(0)
    var maxEnding = 0
    for (i <- 0 until nums.size) {
      //      maxEnding = Math.max(maxEnding, 0) + nums(i)
      maxEnding = Math.max(nums(i), nums(i) + maxEnding)
      if (maxEnding > maxSum) {
        maxSum = maxEnding
      }
    }
    maxSum
  }

  def main(args: Array[String]): Unit = {
    //    val nums = Array(-2, 1, -3, 4, -1, 2, 1, -5, 4)
        val nums = Array(5, -1, 3)
//    val nums = Array(-1, -5, -2, -1)
    println(getMaxSubArr(nums))
    println(getMaxSubArrDP(nums))
  }

}

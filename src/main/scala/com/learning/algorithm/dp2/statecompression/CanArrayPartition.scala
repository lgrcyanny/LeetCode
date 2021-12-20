package com.learning.algorithm.dp2.statecompression

object CanArrayPartition {

  def canPartitionKSubsets(nums: Array[Int], k: Int): Boolean = {
    if (k == 1) {
      true
    } else {
      val n = nums.size
      val sum = nums.sum
      if (sum % k != 0) {
        false
      } else {
        val target = sum / k
        val sortedArr = nums.sorted
        val statesSize = 1 << n
        val dp = Array.ofDim[Boolean](statesSize)
        val currentSum = Array.ofDim[Int](statesSize)
        dp(0) = true
        for (i <- 0 until statesSize) {
          if (dp(i)) {
            var j = 0
            var isStop = false
            while (j < n && !isStop) {
              if ((i & (1 << j)) == 0) {
                val nextState = i | (1 << j)
                if (!dp(nextState)) {
                  if ((currentSum(i) % target) + sortedArr(j) <= target) {
                    currentSum(nextState) = currentSum(i) + sortedArr(j)
                    dp(nextState) = true
                  } else {
                    isStop = true
                  }
                }
              }
              j = j + 1
            }
          }
        }
        dp(statesSize - 1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(2, 2, 2, 2, 3, 4, 5)
    println(canPartitionKSubsets(nums, 4))
  }

}

package com.learning.algorithm.dp2.linear.double.lcs

object LongestCommonSubArr {

  def findLength(nums1: Array[Int], nums2: Array[Int]): Int = {
    val m = nums1.size
    val n = nums2.size
    val dp = Array.ofDim[Int](m + 1, n + 1)
    var res = 0
    for (i <- 0 to m) {
      dp(i)(0) = 0
    }
    for (j <- 0 to n) {
      dp(0)(j) = 0
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (nums1(i - 1) == nums2(j - 1)) {
          dp(i)(j) = dp(i - 1)(j - 1) + 1
        } else {
          dp(i)(j) = 0
        }
        res = Math.max(res, dp(i)(j))
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val nums1 = Array(1, 2, 3, 2, 1)
    val nums2 = Array(3, 2, 1, 4, 7)
    println(findLength(nums1, nums2))
  }

}

package com.learning.algorithm.dp2.linear.single.withother

import scala.collection.mutable.ArrayBuffer

object DivisibleSubset {

  def largestDivisibleSubset(nums: Array[Int]): List[Int] = {
    val n = nums.size
    if (n == 0) {
      Nil
    } else if (n == 1) {
      List(nums(0))
    } else {
      val sortedNums = nums.sorted
      val dp = Array.ofDim[ArrayBuffer[Int]](n)
      for (i <- 0 until n) {
        dp(i) = new ArrayBuffer[Int]()
      }
      dp(0).append(sortedNums(0))
      var maxLen = 1
      for (i <- 1 until n) {
        var j = i - 1
        while (j >= 0) {
          if ((sortedNums(i) % dp(j).last== 0) && dp(i).size < dp(j).size) {
            dp(i).clear()
            dp(j).copyToBuffer(dp(i))
          }
          j = j - 1
        }
        dp(i).append(sortedNums(i))
        if (dp(i).size > maxLen) {
          maxLen = dp(i).size
        }
      }
      println(s"max len ${maxLen}")
      dp.find(_.size == maxLen).get.toList
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(4, 8, 10, 240)
    println(largestDivisibleSubset(nums).mkString(", "))
  }

}

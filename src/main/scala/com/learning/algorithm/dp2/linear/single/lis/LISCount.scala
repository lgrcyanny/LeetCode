package com.learning.algorithm.dp2.linear.single.lis

object LISCount {

  def findNumberOfLIS(nums: Array[Int]): Int = {
    val n = nums.length
    val lengths = Array.ofDim[Int](n)
    val counts = Array.ofDim[Int](n)
    lengths(0) = 1
    for (i <- 0 until n) {
      counts(i) = 1
    }
    for (i <- 1 until n) {
      lengths(i) = 1
      for (j <- 0 until i) {
        if (nums(j) < nums(i)) {
          if (lengths(j) + 1 > lengths(i)) {
            lengths(i) = lengths(j) + 1
            counts(i) = counts(j)
          } else if (lengths(j) + 1 == lengths(i)) {
            counts(i) = counts(i) + counts(j)
          }
        }
      }
    }
    val longest = lengths.max
    val numOfLis = lengths.zip(counts).filter(_._1 == longest).map(_._2).sum
    numOfLis
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(1, 3, 5, 4, 7)
    println(findNumberOfLIS(arr))
  }

}

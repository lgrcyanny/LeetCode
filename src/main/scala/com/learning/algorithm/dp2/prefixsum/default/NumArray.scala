package com.learning.algorithm.dp2.prefixsum.default

object NumArray {

  class NumArray(_nums: Array[Int]) {
    val n = _nums.size
    val prefix = Array.ofDim[Int](n + 1)
    buildPrefixSum()

    private def buildPrefixSum(): Unit = {
      prefix(0) = 0
      for (i <- 1 to n) {
        prefix(i) = prefix(i - 1) + _nums(i - 1)
      }
    }

    def sumRange(left: Int, right: Int): Int = {
      prefix(right + 1) - prefix(left)
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = new NumArray(Array(-2, 0, 3, -5, 2, -1))
    println(arr.sumRange(0, 1))
  }


}

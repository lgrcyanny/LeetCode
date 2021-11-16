package com.learning.algorithm.dp2.linear.single.maxsub

object MaxSumSubMatrix {

  def maxSumSubmatrix(matrix: Array[Array[Int]], k: Int): Int = {
    val N = matrix.length
    val M = matrix(0).length
    val colSum = Array.ofDim[Int](M)
    var maxSum = Int.MinValue
    for (i <- 0 until N) {
      for (t <- 0 until M) {
        colSum(t) = 0
      }
      for (j <- i until N) {
        for (c <- 0 until M) {
          colSum(c) = colSum(c) + matrix(j)(c)
        }
        maxSum = Math.max(maxSum, maxValue(colSum, k))
        println(s"maxSum ${maxSum}, (${colSum.mkString(", ")})")
      }
    }
    maxSum
  }

  def maxValue(nums: Array[Int], k: Int): Int = {
    var temp = 0
    var max = Int.MinValue
    for (i <- 0 until nums.size) {
      temp = 0
      for (j <- i until nums.size) {
        temp = temp + nums(j)
        if (temp <= k) {
          max = Math.max(max, temp)
        }
      }
    }
    max
  }

  def main(args: Array[String]): Unit = {
    //    val matrix = Array(
    //      Array(5, -4, -3, 4),
    //      Array(-3, -4, 4, 5),
    //      Array(5, 1, 5, -4)
    //    )
    val matrix = Array(Array(2, 2, -1))
    //    val matrix = Array(
    //      Array(27,5,-20,-9,1,26,1,12,7,-4,8,7,-1,5,8),
    //      Array(16,28,8,3,16,28,-10,-7,-5,-13,7,9,20,-9,26),
    //      Array(24,-14,20,23,25,-16,-15,8,8,-6,-14,-6,12,-19,-13),
    //      Array(28,13,-17,20,-3,-18,12,5,1,25,25,-14,22,17,12),
    //      Array(7,29,-12,5,-5,26,-5,10,-5,24,-9,-19,20,0,18),
    //      Array(-7,-11,-8,12,19,18,-15,17,7,-1,-11,-10,-1,25,17),
    //      Array(-3,-20,-20,-7,14,-12,22,1,-9,11,14,-16,-5,-12,14),
    //      Array(-20,-4,-17,3,3,-18,22,-13,-1,16,-11,29,17,-2,22),
    //      Array(23,-15,24,26,28,-13,10,18,-6,29,27,-19,-19,-8,0),
    //      Array(5,9,23,11,-4,-20,18,29,-6,-4,-11,21,-6,24,12),
    //      Array(13,16,0,-20,22,21,26,-3,15,14,26,17,19,20,-5),
    //      Array(15,1,22,-6,1,-9,0,21,12,27,5,8,8,18,-1),
    //      Array(15,29,13,6,-11,7,-6,27,22,18,22,-3,-9,20,14),
    //      Array(26,-6,12,-10,0,26,10,1,11,-10,-16,-18,29,8,-8),
    //      Array(-19,14,15,18,-10,24,-9,-7,-19,-14,23,23,17,-5,6)
    //  )
    println(maxSumSubmatrix(matrix, 0))
  }

}

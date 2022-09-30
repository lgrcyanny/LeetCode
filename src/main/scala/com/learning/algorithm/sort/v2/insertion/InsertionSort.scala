package com.learning.algorithm.sort.v2.insertion

object InsertionSort {

  def swap(arr: Array[Int], m: Int, n: Int): Unit = {
    val t = arr(m)
    arr(m) = arr(n)
    arr(n) = t
  }

  def sortBySwap(nums: Array[Int]): Unit = {
    for (i <- 1 until nums.length) {
      for (j <- i until 0 by - 1) {
        if (nums(j) < nums(j - 1)) {
          swap(nums, j, j - 1)
        }
      }
    }
  }

  def sortByMove(nums: Array[Int]): Unit = {
    for (i <- 1 until nums.length) {
      val current = nums(i)
      var targetIndex = i
      for (j <- i - 1 to 0 by -1) {
        if (current < nums(j)) {
          nums(j + 1) = nums(j)
          targetIndex = j
        }
      }
      nums(targetIndex) = current
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(6, 5, 1, 2, 3, 4)
    sortByMove(arr)
    println(arr.mkString(", "))
  }

}

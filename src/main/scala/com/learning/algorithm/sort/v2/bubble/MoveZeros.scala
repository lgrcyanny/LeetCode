package com.learning.algorithm.sort.v2.bubble

object MoveZeros {
  def swap(nums: Array[Int], m: Int, n: Int): Unit = {
    val t = nums(m)
    nums(m) = nums(n)
    nums(n) = t
  }

  def sort(nums: Array[Int]): Unit = {
    var swapped = true
    var lastUnsortedIndex = nums.length - 1
    while (swapped) {
      swapped = false
      var swapIndex = -1
      for (j <- 0 until lastUnsortedIndex) {
        if (compare(nums(j + 1), nums(j)) < 0) {
          swap(nums, j, j + 1)
          swapIndex = j
          swapped = true
        }
      }
      lastUnsortedIndex = swapIndex
    }
  }

  def compare(p: Int, q: Int): Int = {
    if (q == 0) {
      -1
    } else {
      0
    }
  }

  def moveZeroes(nums: Array[Int]): Unit = {
    sort(nums)
  }

  def main(args: Array[String]): Unit = {
    var arr = Array(0, 2)
    moveZeroes(arr)
    println(arr.mkString(", "))
    arr = Array(0, 30, 0, 5, 9)
    moveZeroes(arr)
    println(arr.mkString(", "))
  }
}

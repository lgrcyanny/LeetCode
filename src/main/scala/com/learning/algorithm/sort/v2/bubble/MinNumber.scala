package com.learning.algorithm.sort.v2.bubble

object MinNumber {

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
        if (lessThan(nums(j + 1), nums(j))) {
          swap(nums, j, j + 1)
          swapIndex = j
          swapped = true
        }
      }
      lastUnsortedIndex = swapIndex
    }
  }

  def lessThan(p: Int, q: Int): Boolean = {
    val pstr = p.toString
    val qstr = q.toString
    val numa = pstr + qstr
    val numb = qstr + pstr
    if (numa < numb) {
      true
    } else {
      false
    }
  }

  def minNumber(nums: Array[Int]): String = {
    sort(nums)
    nums.mkString("")
  }

  def main(args: Array[String]): Unit = {
    var arr = Array(10, 2)
    println(minNumber(arr))
    arr = Array(3, 30, 34, 5, 9)
    println(minNumber(arr))
  }


}

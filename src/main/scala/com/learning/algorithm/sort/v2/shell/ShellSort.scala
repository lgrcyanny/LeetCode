package com.learning.algorithm.sort.v2.shell

import com.learning.algorithm.sort.v2.selection.SelectionSort.sort

object ShellSort {

  def sort(nums: Array[Int]): Unit = {
    var gap = nums.length / 2
    while (gap > 0) {
      for (i <- gap until nums.length) {
        val current = nums(i)
        var preIndex = i - gap
        while (preIndex >= 0 && current < nums(preIndex)) {
          nums(preIndex + gap) = nums(preIndex)
          preIndex = preIndex - gap
        }
        nums(preIndex + gap) = current
      }
      gap = gap / 2
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(6, 5, 4, 1, 2, 3)
    sort(arr)
    println(arr.mkString(", "))
  }

}

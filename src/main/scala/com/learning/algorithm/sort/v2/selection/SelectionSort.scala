package com.learning.algorithm.sort.v2.selection

object SelectionSort {

  def swap(arr: Array[Int], m: Int, n: Int): Unit = {
    val t = arr(m)
    arr(m) = arr(n)
    arr(n) = t
  }

  def sort(arr: Array[Int]): Unit = {
    for (i <- 0 until arr.length - 1) {
      var minIndex = i
      for (j <- i + 1 until arr.length) {
        if (arr(minIndex) > arr(j)) {
          minIndex = j
        }
      }
      // swap
      if (minIndex != i) {
        swap(arr, i, minIndex)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(6, 5, 4, 1, 2, 3)
    sort(arr)
    println(arr.mkString(", "))
  }

}

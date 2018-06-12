package com.learning.algorithm.search

/**
  * binary search example
  */
object BinarySearch {

  def search(xs: Array[Int], key: Int, left: Int, right: Int): Int = {
    if (left < right) {
      val mid = (left + right) / 2
      if (xs(mid) == key) {
        mid
      } else if (xs(mid) < key) {
        search(xs, key, mid + 1, right)
      } else {
        search(xs, key, left, mid - 1)
      }
    } else {
      -1
    }
  }

  def search(xs: Array[Int], key: Int): Int = {
    search(xs, key, 0, xs.length)
  }

  def main(args: Array[String]): Unit = {
    val xs = Array(-10, 1, 9, 10, 11, 29, 29, 30, 32, 40, 100, 101)
    println(search(xs, 29))
    println(search(xs, 0))
    println(search(xs, 101))
  }

}

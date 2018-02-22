package com.learning.algorithm.sort

import scala.collection.mutable.ArrayBuffer

object SimpleQuickSort {

  def swap(xs: ArrayBuffer[Int], p: Int, q: Int) = {
    val t = xs(p)
    xs(p) = xs(q)
    xs(q) = t
  }

  def sort(xs: ArrayBuffer[Int]) = {
    def partition(p: Int, r: Int): Int = {
      val pivot = xs(r)
      var i = p
      var j = i - 1
      while (i < r) {
        if (xs(i) < pivot) {
          j = j + 1
          swap(xs, i, j)
        }
        i = i + 1
      }
      j = j + 1
      swap(xs, j, r)
      j
    }

    def qsort(p: Int, r: Int): Unit = {
      if (p < r) {
        val middle = partition(p, r)
        qsort(p, middle - 1)
        qsort(middle + 1, r)
      }
    }
    qsort(0, xs.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val xs = new ArrayBuffer[Int]()
    xs ++= Array(1, 6, 8, 9, 1, 3, 2, 4)
    sort(xs)
    println(xs)
  }
}

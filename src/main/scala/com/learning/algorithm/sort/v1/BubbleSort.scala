package com.learning.algorithm.sort.v1

import scala.collection.mutable.ArrayBuffer

/**
 * Created by lgrcyanny on 17/8/28.
 * Insertion sort has better performance than bubblesort
 */
object BubbleSort {
  def sort(xs: ArrayBuffer[Int]) = {
    def swap(m: Int, n: Int) = {
      val t = xs(m)
      xs.update(m, xs(n))
      xs.update(n, t)
    }

    var swapped = true
    var j = xs.length
    while (swapped) {
      var p = 1
      swapped = false
      while (p < j) {
        if (xs(p - 1) > xs(p)) {
          swap(p - 1, p)
          swapped = true
        }
        p = p + 1
      }
      j = j - 1
    }
  }

  def main(args: Array[String]): Unit = {
    // when list is small
    val buffer = new ArrayBuffer[Int]()
    (1 to 10000).map(x => (Math.random() * 1000 + 1).toInt).foreach(x => buffer.append(x))
    println(buffer.toList)
    val time = System.currentTimeMillis()
    sort(buffer)
    println(buffer.toList)
    println(s"duration: ${System.currentTimeMillis() - time}ms") // 4956ms
  }

}

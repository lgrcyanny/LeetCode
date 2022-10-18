package com.learning.algorithm.sort.v1

import scala.collection.mutable.ArrayBuffer

/**
 * Created by lgrcyanny on 17/8/28.
 */
object QuickSort {

  def swap(xs: ArrayBuffer[Int], m: Int, n: Int) = {
    val t = xs(m)
    xs.update(m, xs(n))
    xs.update(n, t)
  }

  def partition(xs: ArrayBuffer[Int], p: Int, r: Int) = {
    val x = xs(r)
    var i = p - 1
    for (j <- p to r - 1) {
      if (xs(j) <= x) {
        i = i + 1
        swap(xs, i, j)
      }
    }
    swap(xs, r, i + 1)
    i + 1
  }

  def qsort(xs: ArrayBuffer[Int]) = {
    def sortInternal(p: Int, r: Int): Unit = {
      if (p < r) {
        val pivot = partition(xs, p, r)
        sortInternal(p, pivot - 1)
        sortInternal(pivot + 1, r)
      }
    }

    sortInternal(0, xs.length - 1)
  }

  def main(args: Array[String]): Unit = {
    // when list is small
    val buffer = new ArrayBuffer[Int]()
    (1 to 10000).map(x => (Math.random() * 1000 + 1).toInt).foreach(x => buffer.append(x))
    //    List(248, 959, 545, 204, 760, 403, 600, 504, 482, 91).foreach(x => buffer.append(x))
    println(buffer.toList)
    val time = System.currentTimeMillis()
    qsort(buffer)
    println(buffer.toList)
    println(s"duration: ${System.currentTimeMillis() - time}ms") // 10000element in 58ms
  }

}

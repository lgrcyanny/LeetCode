package com.learning.algorithm.sort.v1

import scala.collection.mutable.ArrayBuffer

/**
 * Created by lgrcyanny on 17/8/28.
 */
object InsertionSort {
  def sort(xs: ArrayBuffer[Int]) = {
    var i = 1
    while (i < xs.length) {
      val p = xs(i)
      var j = i - 1
      while (j >= 0 && p < xs(j)) {
        xs.update(j + 1, xs(j))
        j = j - 1
      }
      xs.update(j + 1, p)
      i = i + 1
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
    println(s"duration: ${System.currentTimeMillis() - time}ms") // 936ms
  }

}

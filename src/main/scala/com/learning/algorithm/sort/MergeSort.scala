package com.learning.algorithm.sort

/**
  * Created by lgrcyanny on 17/8/25.
  */
object MergeSort {
  // sort but will has stack overflow problem
  def sort(list: List[Int]): List[Int] = {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = {
      (xs, ys) match {
        case (xs, Nil) => xs
        case (Nil, ys) => ys
        case (x:: xs1, y :: ys1) => {
          if (x < y) {
            x :: merge(xs1, ys)
          } else {
            y :: merge(xs, ys1)
          }
        }
        case _ => throw new RuntimeException("unable to match")
      }
    }
    val middle = list.length / 2
    if (middle == 0) {
      list
    } else {
      val (xs, ys) = list.splitAt(middle)
      val left = sort(xs)
      val right = sort(ys)
      merge(left, right)
    }
  }


  // sort with tail recursive to solve stack overflow problem
  // https://stackoverflow.com/questions/2201472/merge-sort-from-programming-scala-causes-stack-overflow
  def sortOpt(list: List[Int]): List[Int] = {
    def merge(xs: List[Int], ys: List[Int], sortedBuffer: List[Int]): List[Int] = {
      (xs, ys) match {
        case (xs, Nil) => xs.reverse ::: sortedBuffer // the sortedBuffer is reverse
        case (Nil, ys) => ys.reverse ::: sortedBuffer
        case (x:: xs1, y :: ys1) => {
          if (x < y) {
            merge(xs1, ys, x :: sortedBuffer)
          } else {
            merge(xs, ys1, y :: sortedBuffer)
          }
        }
        case _ => throw new RuntimeException("unable to match")
      }
    }
    val middle = list.length / 2
    if (middle == 0) {
      list
    } else {
      val (xs, ys) = list.splitAt(middle)
      val left = sortOpt(xs)
      val right = sortOpt(ys)
      merge(left, right, Nil).reverse
    }
  }


  def main(args: Array[String]): Unit = {
    // when list is small
    val list = (1 to 100).map(x => (Math.random() * 1000 + 1).toInt).toList
    var time = System.currentTimeMillis()
    println(sort(list))
    println(s"small list time = ${System.currentTimeMillis() - time}ms") // 71ms

    // when list is large, 10000 int elements, there will be stack overflow exception
    time = System.currentTimeMillis()
    val largeList = (1 to 100000).map(x => (Math.random() * 1000 + 1).toInt).toList
    println(sortOpt(largeList))
    println(s"large list time = ${System.currentTimeMillis() - time}ms") // 465ms

  }

}

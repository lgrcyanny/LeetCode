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

  /**
    * merge sort for generic types
    */
  def mergeSort[A: Manifest](xs: Array[A])(implicit ord: Ordering[A]) = {
    def internalMerge(a: Array[A], i0: Int, m: Int, iN: Int): Unit = {
      val left = new Array[A](m - i0 + 1)
      val right = new Array[A](iN - m)
      var i = i0
      var j = 0
      var k = 0
      while (i <= m) {
        left(k) = a(i)
        i += 1
        k += 1
      }
      i = m + 1
      k = 0
      while (i <= iN) {
        right(k) = a(i)
        i += 1
        k += 1
      }
      i = 0
      k = i0
      while (k <= iN && i < left.length && j < right.length) {
        if (ord.lt(left(i), right(j))) {
          a(k) = left(i)
          i += 1
        } else {
          a(k) = right(j)
          j += 1
        }
        k += 1
      }
      while (k <= iN && i < left.length) {
        a(k) = left(i)
        k += 1
        i += 1
      }
      while (k <= iN && j < right.length) {
        a(k) = right(j)
        k += 1
        j += 1
      }
    }

    def internalSort(a: Array[A], i0: Int, iN: Int): Unit = {
      if (iN > i0) {
        val m = (iN - i0) / 2
        internalSort(a, i0, m + i0)
        internalSort(a, i0 + m + 1, iN)
        internalMerge(a, i0, i0 + m, iN)
      }
    }
    internalSort(xs, 0, xs.length - 1)
  }


  def main(args: Array[String]): Unit = {
    // when list is small
    val list = (1 to 100).map(x => (Math.random() * 1000 + 1).toInt).toList
    var time = System.currentTimeMillis()
    println(sort(list))
    println(s"small list time = ${System.currentTimeMillis() - time}ms") // 71ms

    // when list is large, 10000 int elements, there will be stack overflow exception
    time = System.currentTimeMillis()
    val largeList = (1 to 10000).map(x => (Math.random() * 1000 + 1).toInt).toList
    println(sortOpt(largeList))
    println(s"large list time = ${System.currentTimeMillis() - time}ms") // 243ms

    // another merge sort algorithm
    time = System.currentTimeMillis()
    val xs = (1 to 100).map(x => (Math.random() * 1000 + 1).toInt).toArray
    mergeSort(xs)
    println(xs.mkString(" "))
    println(s"another algorithm duration: ${System.currentTimeMillis() - time}ms") // 42ms
  }

}

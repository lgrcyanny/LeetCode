package com.learning.algorithm.dp.lcs

import scala.collection.mutable.ArrayBuffer

/**
 * Longest Monotonically increasing subsequence for an int array
 * input: {3, 10, 2, 1, 20}, output is 3, {3, 10, 20}
 * https://www.geeksforgeeks.org/longest-increasing-subsequence-dp-3/
 */
object LIS {

  def getLISRecur(xs: Array[Int]): Int = {
    // L(i) is lis count from 0 to i
    // L(i) = 1 + max(L(j)), 0 <= j < i; L(i) = 1 if j not exist
    def recur(i: Int): Int = {
      if (i == 0) {
        1
      } else {
        var maxLis = 1
        var j = i - 1
        while (j >= 0) {
          val res = recur(j)
          if (xs(j) < xs(i) && (res + 1 > maxLis)) {
              maxLis = res + 1
          }
          j = j - 1
        }
        maxLis
      }
    }
    recur(xs.size - 1)
  }

  def getLisArr(xs: Array[Int], lis: Int): Array[Array[Int]] = {
    val n = xs.size
    // lisArr[i] is lcs from 0 to i
    val lisArr = new ArrayBuffer[ArrayBuffer[Int]]()
    for (i <- 0 until n) {
      lisArr.append(new ArrayBuffer[Int]())
    }
    lisArr(0).append(xs(0))
    for (i <- 1 until n) {
      for (j <- 0 until i) {
        if (xs(i) > xs(j) && xs(i) > lisArr(j).last && lisArr(j).size + 1 > lisArr(i).size) {
          lisArr(i) = lisArr(j)
        }
      }
      lisArr(i).append(xs(i)) // lis arr ends with xs[i]
    }
    val res = lisArr.distinct.filter(_.size == lis).map(_.toArray).toArray
    res
  }

  /**
   * O(n ^ 2)
   */
  def getLisDP(xs: Array[Int]): Int = {
    val n = xs.size
    // memo[i] is lcs from 0 to i
    val memo = Array.ofDim[Int](n)
    memo(0) = 1
    for (i <- 1 until n) {
      memo(i) = 1
      for (j <- 0 until i) {
        if (xs(i) > xs(j) && memo(j) + 1 > memo(i)) {
          memo(i) = memo(j) + 1
        }
      }
    }
    val lis = memo.max
    val lisArr = getLisArr(xs, lis)
    lisArr.foreach { res =>
      println(s"{${res.mkString(", ")}}")
    }
    lis
  }

  /**
   * O(nlog(n)): https://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n/
   */
  def getLISOpt(xs: Array[Int]): Int = {
    def _ceilIndex(arr: ArrayBuffer[Int], left: Int, right: Int, key: Int): Int = {
      var l = left
      var r = right
      var m = (r + l) / 2
      while (r - l > 1) {
        m = (r + l) / 2
        if (arr(m) >= key) {
          r = m
        } else {
          l = m
        }
      }
      r
    }
    val tailTable = new ArrayBuffer[Int]()
    tailTable.append(xs(0))
    var lisLen = 1
    for (i <- 1 until xs.size) {
      if (xs(i) < tailTable(0)) {
        tailTable(0) = xs(i)
      } else if (xs(i) > tailTable(lisLen - 1)) {
        tailTable.append(xs(i))
        lisLen = lisLen + 1
      } else {
        val midIndex = _ceilIndex(tailTable, -1, lisLen - 1, xs(i))
        tailTable(midIndex) = xs(i)
      }
    }
    println(s"lcs seq: {${tailTable.mkString(", ")}}")
    lisLen
  }

  def main(args: Array[String]): Unit = {
    val arr = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)
//    val arr = Array(2, 5, 3, 7, 11, 8, 10, 13, 6)
    println(s"lis count recur ${getLISRecur(arr)}")
    println(s"lis count dp ${getLisDP(arr)}")
    println(s"lis count opt ${getLISOpt(arr)}")
  }

}

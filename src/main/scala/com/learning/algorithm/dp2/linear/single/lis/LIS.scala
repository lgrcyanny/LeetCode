package com.learning.algorithm.dp2.linear.single.lis

import scala.collection.mutable.ArrayBuffer

/**
 * f(n) = max(f(i)) + 1, i < n, arr[i] < arr[n]
 */
object LIS {

  def getLISLength(arr: Array[Int]): Int = {
    def _recur(i: Int): Int = {
      var len = 1
      for (j <- 0 until i) {
        if (arr(j) < arr(i)) {
          val t = _recur(j) + 1
          if (t > len) {
            len = t
          }
        }
      }
      len
    }

    val n = arr.length
    val res = for (i <- 1 until n) yield _recur(i)
    res.max
  }

  def getLISLengthDP(arr: Array[Int]): Int = {
    val n = arr.size
    val memo = Array.ofDim[Int](n) // memo[i] is LIS from 0 to i
    memo(0) = 1
    var maxLen = 1
    for (i <- 1 until n) {
      memo(i) = 1
      for (j <- 0 until i) {
        if (arr(j) < arr(i) && memo(j) + 1 > memo(i)) {
          memo(i) = memo(j) + 1
        }
      }
      if (memo(i) > maxLen) {
        maxLen = memo(i)
      }
    }
    //    println(memo.mkString(", "))
    maxLen
  }

  def getLISLengthOpt(nums: Array[Int]): Int = {
    val n = nums.length
    if (n == 0) {
      0
    } else {
      val tailTable = new ArrayBuffer[Int]()
      tailTable.append(nums(0))
      for (i <- 1 until n) {
        if (nums(i) < tailTable.head) {
          tailTable(0) = nums(i)
        } else if (nums(i) > tailTable.last) {
          tailTable.append(nums(i))
        } else {
          // binary search
          var l = 0
          var r = tailTable.length - 1
          while (l + 1 < r) {
            val middle = (l + r) / 2
            if (tailTable(middle) <= nums(i)) {
              l = middle
            } else {
              r = middle
            }
          }
          tailTable(r) = nums(i)
        }
      }
      println(tailTable.mkString(","))
      tailTable.length
    }
  }


  def main(args: Array[String]): Unit = {
    val arr = Array(1, 3, 6, 7, 9, 4, 10, 5, 6)
    //        val arr = Array(10,9,2,5,3,7,101,18, 1, 2, 4)
    println(getLISLength(arr))
    println(getLISLengthDP(arr))
    println(getLISLengthOpt(arr))
  }

}

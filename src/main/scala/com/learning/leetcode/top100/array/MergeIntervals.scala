package com.learning.leetcode.top100.array

import scala.collection.mutable.ArrayBuffer

object MergeIntervals {
  /**
   * 以数组 intervals 表示若干个区间的集合，其中单个区间为 intervals[i] = [starti, endi] 。请你合并所有重叠的区间，并返回 一个不重叠的区间数组，该数组需恰好覆盖输入中的所有区间 。
   * 示例 1：
   *
   * 输入：intervals = [[1,3],[2,6],[8,10],[15,18]]
   * 输出：[[1,6],[8,10],[15,18]]
   * 解释：区间 [1,3] 和 [2,6] 重叠, 将它们合并为 [1,6].
   * 示例 2：
   *
   * 输入：intervals = [[1,4],[4,5]]
   * 输出：[[1,5]]
   * 解释：区间 [1,4] 和 [4,5] 可被视为重叠区间。
   */
  def merge(intervals: Array[Array[Int]]): Array[Array[Int]] = {
    val sortedIntervals = intervals.sortBy(_.head)
    val res = new ArrayBuffer[Array[Int]]()
    for (interval <- sortedIntervals) {
      if (res.isEmpty || res.last(1) < interval(0)) {
        res.append(interval)
      } else {
        res.last(1) = Math.max(res.last(1), interval(1))
      }
    }
    res.toArray
  }

  def main(args: Array[String]): Unit = {
    val intervals = Array(
      Array(1, 3),
      Array(2, 6),
      Array(8, 10),
      Array(15, 18)
    )
    val res = merge(intervals)
    res.foreach { interval =>
      println(interval.mkString(", "))
    }
  }

}

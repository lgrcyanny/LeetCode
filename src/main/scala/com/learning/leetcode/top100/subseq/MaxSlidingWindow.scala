package com.learning.leetcode.top100.subseq

import scala.collection.mutable

object MaxSlidingWindow {
  /**
   * 滑动窗口的最大值
   * 给你一个整数数组 nums，有一个大小为 k 的滑动窗口从数组的最左侧移动到数组的最右侧。你只可以看到在滑动窗口内的 k 个数字。滑动窗口每次只向右移动一位。
   *
   * 返回 滑动窗口中的最大值 。
   * 示例 1：
   *
   * 输入：nums = [1,3,-1,-3,5,3,6,7], k = 3
   * 输出：[3,3,5,5,6,7]
   * 解释：
   * 滑动窗口的位置                最大值
   * ---------------               -----
   * [1  3  -1] -3  5  3  6  7       3
   * 1 [3  -1  -3] 5  3  6  7       3
   * 1  3 [-1  -3  5] 3  6  7       5
   * 1  3  -1 [-3  5  3] 6  7       5
   * 1  3  -1  -3 [5  3  6] 7       6
   * 1  3  -1  -3  5 [3  6  7]      7
   * 示例 2：
   *
   * 输入：nums = [1], k = 1
   * 输出：[1]
   */

  /**
   * 双端单调队列，元素是递减的, 队列中放入元素的索引
   * 最大值是队列头部，滑窗时，判断队首索引是否在滑窗外，出队
   */
  def maxSlidingWindow(nums: Array[Int], k: Int): Array[Int] = {
    val ans = new mutable.ArrayBuffer[Int]()
    val dequeue = new mutable.ArrayBuffer[Int]()
    for (i <- 0 until k) {
      while (dequeue.nonEmpty && nums(i) >= nums(dequeue.last)) {
        dequeue.remove(dequeue.length - 1)
      }
      dequeue.append(i)
    }
    ans.append(nums(dequeue.head))
    for (i <- k until nums.length) {
      while (dequeue.nonEmpty && nums(i) >= nums(dequeue.last)) {
        dequeue.remove(dequeue.length - 1)
      }
      dequeue.append(i)
      while (dequeue.head <= i - k) {
        dequeue.remove(0)
      }
      ans.append(nums(dequeue.head))
    }
    ans.toArray
  }

  def main(args: Array[String]): Unit = {
//    val nums = Array(1, 3, -1, -3, 5, 3, 6, 7)
//    val res = maxSlidingWindow(nums, 3)
    val nums = Array(1, 3, 1, 2, 0, 5)
    val res = maxSlidingWindow(nums, 3)
    println(res.mkString(", "))
  }


}

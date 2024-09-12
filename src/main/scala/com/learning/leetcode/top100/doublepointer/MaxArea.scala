package com.learning.leetcode.top100.doublepointer

object MaxArea {
  /**
   * https://leetcode.cn/problems/container-with-most-water/description/?envType=study-plan-v2&envId=top-100-liked
   * 给定一个长度为 n 的整数数组 height 。有 n 条垂线，第 i 条线的两个端点是 (i, 0) 和 (i, height[i]) 。
   *
   * 找出其中的两条线，使得它们与 x 轴共同构成的容器可以容纳最多的水。
   *
   * 返回容器可以储存的最大水量。
   *
   * 说明：你不能倾斜容器。
   */
  def maxArea(height: Array[Int]): Int = {
    if (height.length <= 1) {
      0
    } else {
      var area = 0
      var i = 0
      var j = height.length - 1
      while (i < j) {
        val areaWidth = j - i
        val areaHeight = Math.min(height(i), height(j))
        area = Math.max(area, areaHeight * areaWidth)
        if (height(i) < height(j)) {
          i = i + 1
        } else {
          j = j - 1
        }
      }
      area
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 8, 6, 2, 5, 4, 8, 3, 7)
    println(maxArea(nums))
  }
}

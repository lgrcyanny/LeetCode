package com.learning.leetcode.top100.doublepointer

object RainTrap {

  /**
   * https://leetcode.cn/problems/trapping-rain-water/description/?envType=study-plan-v2&envId=top-100-liked
   * 给定 n 个非负整数表示每个宽度为 1 的柱子的高度图，计算按此排列的柱子，下雨之后能接多少雨水。
   *
   * 示例 1：
   * 输入：height = [0,1,0,2,1,0,1,3,2,1,2,1]
   * 输出：6
   * 解释：上面是由数组 [0,1,0,2,1,0,1,3,2,1,2,1] 表示的高度图，在这种情况下，可以接 6 个单位的雨水（蓝色部分表示雨水）。
   * 示例 2：
   *
   * 输入：height = [4,2,0,3,2,5]
   * 输出：9
   *
   *
   * 提示：
   *
   * n == height.length
   * 1 <= n <= 2 * 104
   * 0 <= height[i] <= 105
   */

  /**
   * S1:
   *  动态规划，两个数组
   *     leftMax[i], 代表在 i 处，左边的最大高度
   *     rightMax[i], 代表在 i 处，右边的最大高度
   *     当 1≤i≤n−1 时，leftMax[i]=max(leftMax[i−1],height[i])
   *     当 0≤i≤n−2 时，rightMax[i]=max(rightMax[i+1],height[i])
   *     for i to n, trapRes += min(leftMax[i], rightMax[i] - height[i])
   *
   * S2: 单调栈
   * S3: 双指针，优化动态规划的空间复杂度
   */
   def trap(height: Array[Int]): Int = {
     var left = 0
     var right = height.length - 1
     var leftMax = 0
     var rightMax = 0
     var trapRes = 0
     while (left < right) {
       leftMax = Math.max(leftMax, height(left))
       rightMax = Math.max(rightMax, height(right))
       if (height(left) < height(right)) {
         trapRes = trapRes + leftMax - height(left)
         left = left + 1
       } else {
         trapRes = trapRes + rightMax - height(right)
         right = right - 1
       }
     }
     trapRes
   }

  def main(args: Array[String]): Unit = {
    val nums = Array(0,1,0,2,1,0,1,3,2,1,2,1)
    println(trap(nums))

  }

}

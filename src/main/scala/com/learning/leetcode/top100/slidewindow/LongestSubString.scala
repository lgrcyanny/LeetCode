package com.learning.leetcode.top100.slidewindow

import scala.collection.mutable

object LongestSubString {
  /**
   * 无重复字符的最长子串
   *  给定一个字符串 s ，请你找出其中不含有重复字符的 最长
   * 子串
   * 的长度。
   * 示例 1:
   *
   * 输入: s = "abcabcbb"
   * 输出: 3
   * 解释: 因为无重复字符的最长子串是 "abc"，所以其长度为 3。
   * 示例 2:
   *
   * 输入: s = "bbbbb"
   * 输出: 1
   * 解释: 因为无重复字符的最长子串是 "b"，所以其长度为 1。
   * 示例 3:
   *
   * 输入: s = "pwwkew"
   * 输出: 3
   * 解释: 因为无重复字符的最长子串是 "wke"，所以其长度为 3。
   * 请注意，你的答案必须是 子串 的长度，"pwke" 是一个子序列，不是子串。
   *
   */
  def lengthOfLongestSubstring(s: String): Int = {
    var maxLength = 0
    for (i <- 0 until s.length) {
      val visited = new mutable.HashSet[Char]()
      var len = 0
      var j = i
      while (j < s.length && !visited.contains(s.charAt(j))) {
        len = len + 1
        visited.add(s.charAt(j))
        j = j + 1
      }
      maxLength = Math.max(maxLength, len)
    }
    maxLength
  }

  def main(args: Array[String]): Unit = {
    val s = "pwwkew"
    println(lengthOfLongestSubstring(s))
  }

}

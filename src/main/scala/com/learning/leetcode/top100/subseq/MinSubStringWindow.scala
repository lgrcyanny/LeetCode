package com.learning.leetcode.top100.subseq

import scala.collection.mutable

object MinSubStringWindow {
  /**
   * 最小覆盖子串
   * 给你一个字符串 s 、一个字符串 t 。返回 s 中涵盖 t 所有字符的最小子串。如果 s 中不存在涵盖 t 所有字符的子串，则返回空字符串 "" 。
   * 注意：
   *
   * 对于 t 中重复字符，我们寻找的子字符串中该字符数量必须不少于 t 中该字符数量。
   * 如果 s 中存在这样的子串，我们保证它是唯一的答案。
   *
   *
   * 示例 1：
   *
   * 输入：s = "ADOBECODEBANC", t = "ABC"
   * 输出："BANC"
   * 解释：最小覆盖子串 "BANC" 包含来自字符串 t 的 'A'、'B' 和 'C'。
   * 示例 2：
   *
   * 输入：s = "a", t = "a"
   * 输出："a"
   * 解释：整个字符串 s 是最小覆盖子串。
   * 示例 3:
   *
   * 输入: s = "a", t = "aa"
   * 输出: ""
   * 解释: t 中两个字符 'a' 均应包含在 s 的子串中，
   * 因此没有符合条件的子字符串，返回空字符串。
   *
   * 思路: 滑窗检测，滑窗有两个指针，left，right，right右移当发现所有的字母t后，left右移直到找到最小的滑窗。
   *      为方便查找t的字母，用hashmap<char, int>, key是字符，value是字母的count
   */
  def minWindow(s: String, t: String): String = {
    val targetCount = new mutable.HashMap[Char, Int]()
    val windowCount = new mutable.HashMap[Char, Int]()
    def isValid: Boolean = {
      targetCount.forall{case (char, count) => windowCount.getOrElse(char, 0) >= count}
    }
    for (c <- t) {
      targetCount.put(c, targetCount.getOrElse(c, 0) + 1)
    }
    var left = 0
    var right = 0
    var len = Int.MaxValue
    var resLeft = -1
    while (right < s.length) {
      val char = s(right)
      if (targetCount.contains(char)) {
        windowCount.put(char, windowCount.getOrElse(s(right), 0) + 1)
      }
      while (isValid && left <= right) {
        if (right - left + 1 < len) {
          len = right - left + 1
          resLeft = left
        }
        val leftChar = s(left)
        if (targetCount.contains(s(left))) {
          windowCount.put(leftChar, windowCount.getOrElse(leftChar, 0) - 1)
        }
        left = left + 1
      }
      right = right + 1
    }
    if (resLeft == -1) {
      ""
    } else {
      println(s"resLeft ${resLeft}, len ${len}")
      s.substring(resLeft, resLeft + len)
    }
  }

  def main(args: Array[String]): Unit = {
    val s = "a"
    val t = "a"
    println(minWindow(s, t))
  }


}

package com.learning.leetcode.top100.slidewindow

import scala.collection.mutable.ArrayBuffer

object Anagram {
  /**
   * 给定两个字符串 s 和 p，找到 s 中所有 p 的 异位词 的子串，返回这些子串的起始索引。不考虑答案输出的顺序。
   *
   * 异位词 指由相同字母重排列形成的字符串（包括相同的字符串）。
   * 示例 1:
   *
   * 输入: s = "cbaebabacd", p = "abc"
   * 输出: [0,6]
   * 解释:
   * 起始索引等于 0 的子串是 "cba", 它是 "abc" 的异位词。
   * 起始索引等于 6 的子串是 "bac", 它是 "abc" 的异位词。
   * 示例 2:
   *
   * 输入: s = "abab", p = "ab"
   * 输出: [0,1,2]
   * 解释:
   * 起始索引等于 0 的子串是 "ab", 它是 "ab" 的异位词。
   * 起始索引等于 1 的子串是 "ba", 它是 "ab" 的异位词。
   * 起始索引等于 2 的子串是 "ab", 它是 "ab" 的异位词。
   *
   *
   * 提示:
   *
   * 1 <= s.length, p.length <= 3 * 104
   * s 和 p 仅包含小写字母
   *
   * 思路
   *  1.维护两个记录字母数量的数组
   *  2.在s的遍历中，按p的滑窗前进，当数组的相等，就找到异位词
   *  刚开始想检测ascii的量是否相等，但不确定是不是有相同的总数对应不通的字母组合
   */
  import scala.collection.mutable.ArrayBuffer

  def findAnagrams(s: String, p: String): List[Int] = {
    if (s.length < p.length) {
      List.empty
    } else {
      val res = new ArrayBuffer[Int]()
      val countS = Array.fill[Int](26)(0)
      val countP = Array.fill[Int](26)(0)
      for (i <- 0 until p.length) {
        countS(s(i).toInt - 97) = countS(s(i).toInt - 97) + 1
        countP(p(i).toInt - 97) = countP(p(i).toInt - 97) + 1
      }
      if (countS.sameElements(countP)) {
        res.append(0)
      }
      for (i <- 0 until (s.length - p.length)) {
        countS(s(i).toInt - 97) = countS(s(i).toInt - 97) - 1
        countS(s(i + p.length).toInt - 97) = countS(s(i + p.length).toInt - 97) + 1
        if (countS.sameElements(countP)) {
          res.append(i + 1)
        }
      }
      res.toList
    }
  }

  def main(args: Array[String]): Unit = {
    val s = "cbaebabacd"
    val p = "abc"
    println(findAnagrams(s, p))
  }




}

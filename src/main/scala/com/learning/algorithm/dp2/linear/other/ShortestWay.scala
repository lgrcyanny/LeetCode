package com.learning.algorithm.dp2.linear.other

object ShortestWay {

  def longestCommonSubSeqLen(source: String, target: String): Int = {
    val n = source.size
    val m = target.size
    var i = 0
    var j = 0
    var len = 0
    while (i < n && j < m) {
      if (source(i) == target(j)) {
        len = len + 1
        i = i + 1
        j = j + 1
      } else {
        i = i + 1
      }
    }
    len
  }

  /**
   * greedy algorithm
   */
  def shortestWay(source: String, target: String): Int = {
    var targetStr = target
    var len = longestCommonSubSeqLen(source, targetStr)
    var res = 0
    while (len > 0) {
      res = res + 1
      targetStr = targetStr.substring(len)
      len = longestCommonSubSeqLen(source, targetStr)
    }
    if (len == 0 && !targetStr.isEmpty) -1 else res
  }

  /**
   * verify is target subseq of source
   */
  def isSubSeq(source: String, target: String): Boolean = {
    val n = source.size
    val m = target.size
    var i = 0
    var j = 0
    while (i < n && j < m) {
      if (source(i) == target(j)) {
        j = j + 1
      }
      i = i + 1
    }
    j == m
  }

  def shortestWayDP(source: String, target: String): Int = {
    val n = target.size
    val temp = new StringBuilder
    var minCount = 0
    if (source.contains(target(0))) {
      temp.append(target(0))
      minCount = 1
    }
    var i = 1
    while (minCount > 0 && i < n && source.contains(target(i))) {
      temp.append(target(i))
      if (!isSubSeq(source, temp.toString())) {
        temp.clear()
        minCount = minCount + 1
      } else {
        i = i + 1
      }
    }
    if (i < n) -1 else minCount
  }

  def main(args: Array[String]): Unit = {
    val source = "xyz"
    val target = "xzyxz"
    println(shortestWay(source, target))
    println(shortestWayDP(source, target))
  }

}

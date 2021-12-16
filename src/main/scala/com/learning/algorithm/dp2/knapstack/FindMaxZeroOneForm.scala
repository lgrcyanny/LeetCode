package com.learning.algorithm.dp2.knapstack

import scala.collection.mutable

object FindMaxZeroOneForm {

  val strCounts = new mutable.HashMap[String, (Int, Int)]()

  def countStr(s: String): (Int, Int) = {
    if (strCounts.contains(s)) {
      strCounts(s)
    } else {
      val chars = s.toCharArray
      val zeroCount = chars.count(_ == '0')
      val oneCount = chars.count(_ == '1')
      strCounts.put(s, (zeroCount, oneCount))
      (zeroCount, oneCount)
    }
  }

  def findMaxForm(strs: Array[String], m: Int, n: Int): Int = {
    val dp = Array.ofDim[Int](m + 1, n + 1)
    dp(0)(0) = 0
    for (i <- 0 until strs.length) {
      for (p <- m to 0 by -1) {
        for (q <- n to 0 by -1) {
            val (zeroCount, oneCount) = countStr(strs(i))
            if (zeroCount <= p && oneCount <= q) {
              dp(p)(q) = Math.max(dp(p)(q), dp(p - zeroCount)(q - oneCount) + 1)
            }
          }
      }
    }
    dp(m)(n)
  }

  def main(args: Array[String]): Unit = {
//    val strs = Array("10", "0001", "111001", "1", "0")
//    println(findMaxForm(strs, 5, 3)) // expect 4
    val strs = Array("10", "0", "1")
    println(findMaxForm(strs, 1, 1)) // expect 2
  }

}

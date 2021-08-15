package com.learning.algorithm.dp

object StrSumMaxLength {

  /**
   * DP solution O(n ^ 2)
   */
  def process(str: String): Int = {
    val n = str.length
    val sum = Array.ofDim[Int](n, n)
    var maxLength = 0
    for (i <- 0 until n) {
      sum(i)(i) = str(i).toInt
    }
    for (len <- 2 to n) {
      for (i <- 0 until (n - len + 1)) {
        val j = i + len - 1
        val k = len / 2
        sum(i)(j) = sum(i)(j - k) + sum(j - k + 1)(j)
        if ((len % 2 == 0) && (sum(i)(j - k) == sum(j - k + 1)(j)) && len > maxLength) {
          maxLength = len
        }
      }
    }
    maxLength
  }

  def main(args: Array[String]): Unit = {
    println(process("124421"))
    println(process("11345"))
    println(process("9430723"))
  }
}

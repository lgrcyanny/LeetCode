package com.learning.algorithm.dp.lcs

object LPS {

  def lps(str: String, start: Int, end: Int): Int = {
    if (start > end) {
      0
    } else if (start == end) {
      1
    } else if (str(start) == str(end)) {
      2 + lps(str, start + 1, end - 1)
    } else {
      Math.max(lps(str, start + 1, end), lps(str, start, end - 1))
    }
  }

  def lpsDP(str: String): Int = {
    val n = str.length
    val memo = Array.ofDim[Int](n, n)
    for (i <- 0 until n) {
      memo(i)(i) = 1
    }
    for (k <- 2 to n) {
      for (i <- 0 until n - k + 1) {
        val j = i + k - 1
        if (str(i) == str(j) && k == 2) {
          memo(i)(j) = 2
        } else if (str(i) == str(j)) {
          memo(i)(j) = 2 + memo(i + 1)(j - 1)
        } else {
          memo(i)(j) = Math.max(memo(i + 1)(j), memo(i)(j - 1))
        }
      }
    }
    memo(0)(n - 1)
  }

  def lpsDPWithPrinter(str: String): Int = {
    val n = str.length
    val memo = Array.ofDim[Int](n, n)
    val memoStr = Array.ofDim[String](n, n)
    for (i <- 0 until n) {
      memo(i)(i) = 1
      memoStr(i)(i) = str(i).toString
    }
    for (k <- 2 to n) {
      for (i <- 0 until n - k + 1) {
        val j = i + k - 1
        if (str(i) == str(j) && k == 2) {
          memo(i)(j) = 2
          if (memoStr(i)(j) != null) {
            memoStr(i)(j) = s"${str(i)}${str(j)}"
          }
        } else if (str(i) == str(j)) {
          memo(i)(j) = 2 + memo(i + 1)(j - 1)
          memoStr(i)(j) = s"${str(i)}${memoStr(i + 1)(j - 1)}${str(j)}"
        } else {
          if (memo(i + 1)(j) > memo(i)(j - 1)) {
            memo(i)(j) = memo(i + 1)(j)
            memoStr(i)(j) = memoStr(i + 1)(j)
          } else {
            memo(i)(j) = memo(i)(j - 1)
            memoStr(i)(j) = memoStr(i)(j - 1)
          }
        }
      }
    }
    println(memoStr(0)(n - 1))
    memo(0)(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val str = "BBABCBCAB"
    println(lps(str, 0, str.length - 1))
    println(lpsDP(str))
    println(lpsDPWithPrinter(str))
  }

}

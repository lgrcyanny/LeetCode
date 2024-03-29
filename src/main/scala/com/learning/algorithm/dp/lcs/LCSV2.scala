package com.learning.algorithm.dp.lcs

object LCSV2 {

  /**
   * O(2^n)
   */
  def getLCSRecur(s1: String, s2: String): Int = {
    def recur(m: Int, n: Int): Int = {
      if (m == s1.size || n == s2.size) {
        0
      } else if (s1(m) == s2(n)) {
        1 + recur(m + 1, n + 1)
      } else {
        Array(recur(m + 1, n), recur(m, n + 1)).max
      }
    }

    recur(0, 0)
  }

  def getLCSRecurWithMemo(s1: String, s2: String): Int = {
    val m = s1.size
    val n = s2.size
    val memo = Array.ofDim[Int](m + 1, n + 1) // plus one for empty string
    for (i <- 0 to m; j <- 0 to n) {
      memo(i)(j) = -1
    }

    def recur(m: Int, n: Int): Int = {
      if (m == 0 || n == 0) { // empty string
        0
      } else {
        if (memo(m)(n) != -1) {
          memo(m)(n)
        } else {
          if (s1(m - 1) == s2(n - 1)) {
            memo(m)(n) = 1 + recur(m - 1, n - 1)
          } else {
            memo(m)(n) = Array(recur(m - 1, n), recur(m, n - 1)).max
          }
          memo(m)(n)
        }
      }
    }

    recur(m, n)
  }

  def getLCSStr(s1: String, s2: String, lcsCount: Array[Array[Int]]): String = {
    val m = s1.size
    val n = s2.size
    val lcsStr = new StringBuilder
    var i = m
    var j = n
    while (i > 0 && j > 0) {
      if (s1(i - 1) == s2(j - 1)) {
        lcsStr.append(s1(i - 1))
        i = i - 1
        j = j - 1
      } else if (lcsCount(i - 1)(j) > lcsCount(i)(j - 1)) {
        i = i - 1
      } else {
        j = j - 1
      }
    }
    lcsStr.toString().reverse
  }

  def getLCSDP(s1: String, s2: String): String = {
    val m = s1.size
    val n = s2.size
    val memo = Array.ofDim[Int](m + 1, n + 1)
    for (i <- 0 to m) {
      memo(i)(0) = 0
    }
    for (j <- 0 to n) {
      memo(0)(j) = 0
    }
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (s1(i - 1) == s2(j - 1)) {
          memo(i)(j) = 1 + memo(i - 1)(j - 1)
        } else {
          memo(i)(j) = math.max(memo(i)(j - 1), memo(i - 1)(j))
        }
      }
    }
    val lcsStr = getLCSStr(s1, s2, memo)
    lcsStr
  }

  def main(args: Array[String]): Unit = {
        val s1 = "ABCD"
        val s2 = "AEBD"
//    val s1 = "AAACCGTGAGTTATTCGTTCTAGAA"
//    val s2 = "CACCCCTAAGGTACCTTTGGTTC"
    val lcs = getLCSDP(s1, s2)
    println(s"lcs:  ${lcs}, lcs count: ${lcs.size}")
  }

}

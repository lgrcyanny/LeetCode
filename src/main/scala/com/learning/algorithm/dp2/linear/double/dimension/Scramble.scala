package com.learning.algorithm.dp2.linear.double.dimension

object Scramble {
  def isScramble(s1: String, s2: String): Boolean = {
    val n = s1.length
    if (s1.length != s2.length) {
      false
    } else if (n == 1) {
      s1(0) == s2(0)
    } else {
      val dp = Array.ofDim[Boolean](n, n, n + 1)
      for (i <- 0 until n) {
        for (j <- 0 until n) {
          dp(i)(j)(1) = s1(i) == s2(j)
        }
      }

      for (k <- 2 to n) {
        for (i <- 0 to n - k) {
          for (j <- 0 to n - k) {
            for (m <- 1 until k) {
              dp(i)(j)(k) = ((dp(i)(j)(k) ||
                (dp(i)(j)(m) && dp(i + m)(j + m)(k - m)) || (
                (dp(i)(j + k - m)(m) && dp(i + m)(j)(k - m)))))
            }
          }
        }
      }
      dp(0)(0)(n)
    }
  }

  def main(args: Array[String]): Unit = {
    val s1 = "abcde"
    val s2 = "caebd"
    println(isScramble(s1, s2))
  }
}

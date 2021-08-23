package com.learning.algorithm.dp

object StringInterleaving {

  def verifyInterleavingV1(a: String, b: String, c: String): Boolean = {
    def verifyRecur(m: Int, n: Int, q: Int): Boolean = {
      if (m == a.size && n == b.size) {
        true
      } else if ((m == a.size && b(n) == c(q)) || (b(n) == c(q) && a(m) != c(q))) {
        verifyRecur(m, n + 1, q + 1)
      } else if ((n == b.size && a(m) == c(q)) || (a(m) == c(q) && b(n) != c(q))) {
        verifyRecur(m + 1, n, q + 1)
      } else if (a(m) == c(q) && b(n) == c(q)) {
        verifyRecur(m + 1, n, q + 1) || verifyRecur(m, n + 1, q + 1)
      } else {
        false
      }
    }
    verifyRecur(0, 0, 0)
  }

  def verifyInterleavingV2(a: String, b: String, c: String): Boolean = {
    def verifyRecur(m: Int, n: Int, q: Int): Boolean = {
      // if all string is empty
      if (m == 0 && n == 0 && q == 0) {
        true
      } else if (q == 0) { // if c is empty, a or b not empty
        false
      } else if (m == 0 && n == 0) { // if a or b empty, c is not
        false
      } else {
        var first = false
        var second = false
        if (m >= 1 && a(m - 1) == c(q - 1)) {
          first = verifyRecur(m - 1, n, q - 1)
        }
        if (n >= 1 && b(n - 1) == c(q - 1)) {
          second = verifyRecur(m, n - 1, q - 1)
        }
        first || second
      }
    }
    verifyRecur(a.size, b.size, c.size)
  }

  def verifyInterleavingDP(a: String, b: String, c: String): Boolean = {
    val m = a.size
    val n = b.size
    val memo = Array.ofDim[Boolean](m + 1, n + 1)
    if (m + n != c.size) {
      false
    } else {
      memo(0)(0) = true
      for (i <- 1 to m) {
        if (a(i - 1) != c(i - 1)) {
          memo(i)(0) = false
        } else {
          memo(i)(0) = memo(i - 1)(0)
        }
      }
      for (j <- 1 to n) {
        if (b(j - 1) != c(j - 1)) {
          memo(0)(j) = false
        } else {
          memo(0)(j) = memo(0)(j - 1)
        }
      }
      for (i <- 1 to m) {
        for (j <- 1 to n) {
          if (a(i - 1) == c(i + j - 1) && b(j - 1) != c(i + j - 1)) {
            memo(i)(j) = memo(i - 1)(j)
          } else if (b(j - 1) == c(i + j - 1) && a(i - 1) != c(i + j - 1)) {
            memo(i)(j) = memo(i)(j - 1)
          } else if (a(i - 1) == c(i + j - 1) && b(j - 1) == c(i + j - 1)) {
            memo(i)(j) = memo(i - 1)(j) || memo(i)(j - 1)
          } else {
            memo(i)(j) = false
          }
        }
      }
      memo(m)(n)
    }
  }

  def main(args: Array[String]): Unit = {
    println(verifyInterleavingV1("xyz", "abcd", "xabyczd")) // true
    println(verifyInterleavingV2("xyz", "abcd", "xabyczd")) // true
    println(verifyInterleavingDP("xyz", "abcd", "xabyczd")) // true
    println(verifyInterleavingDP("bcc", "bbca", "bbcbcac")) // true
    println(verifyInterleavingDP("bcc", "bbca", "bbrbcar")) // false
  }

}

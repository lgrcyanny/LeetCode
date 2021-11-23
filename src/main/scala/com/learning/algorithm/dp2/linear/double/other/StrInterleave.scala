package com.learning.algorithm.dp2.linear.double.other

object StrInterleave {

  def isInterleave(s1: String, s2: String, s3: String): Boolean = {
    val n1 = s1.length
    val n2 = s2.length
    val n3 = s3.length
    if (n1 + n2 != n3) {
      false
    } else if (n1 == 0 && n2 == 0 && n3 == 0) {
      true
    } else {
      val dp = Array.ofDim[Boolean](n1 + 1, n2 + 1)
      dp(0)(0) = true
      for (i <- 0 to n1) {
        for (j <- 0 to n2) {
          if (i > 0) {
            dp(i)(j) = dp(i)(j) || (dp(i - 1)(j) && s1(i - 1) == s3(i + j - 1))
          }
          if (j > 0) {
            dp(i)(j) = dp(i)(j) || (dp(i)(j - 1) && s2(j - 1) == s3(i + j - 1))
          }
        }
      }
      dp(n1)(n2)
    }
  }

  def isInterleaveOpt(s1: String, s2: String, s3: String): Boolean = {
    val n1 = s1.length
    val n2 = s2.length
    val n3 = s3.length
    if (n1 + n2 != n3) {
      false
    } else if (n1 == 0 && n2 == 0 && n3 == 0) {
      true
    } else {
      val dp = Array.ofDim[Boolean](n2 + 1)
      dp(0) = true
      for (i <- 0 to n1) {
        for (j <- 0 to n2) {
          val p = i + j - 1
          if (i > 0) {
            dp(j) = dp(j) && s1(i - 1) == s3(p)
          }
          if (j > 0) {
            dp(j) = dp(j) || (dp(j - 1) && s2(j - 1) == s3(p))
          }
        }
      }
      dp(n2)
    }
  }

  def main(args: Array[String]): Unit = {
    val s1 = "aabcc"
    val s2 = "dbbca"
    val s3 = "aadbbcbcac"
    println(isInterleave(s1, s2, s3))
    println(isInterleaveOpt(s1, s2, s3))
  }

}

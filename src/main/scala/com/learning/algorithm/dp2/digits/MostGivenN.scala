package com.learning.algorithm.dp2.digits

object MostGivenN {
  def atMostNGivenDigitSet(digits: Array[String], N: Int): Int = {
    val nStr = N.toString
    val dLen = digits.size
    val digitsValue = digits.map(_.toInt)
    val K = nStr.size
    val dp = Array.ofDim[Int](K + 1)
    dp(K) = 1
    for (i <- K - 1 to 0 by -1) {
      val iValue = nStr(i) - '0'
      for (d <- digitsValue) {
        if (d < iValue) {
          dp(i) = dp(i) + Math.pow(dLen, K - i - 1).toInt
        } else if (d == iValue) {
          dp(i) = dp(i) + dp(i + 1)
        }
      }
    }

    for (i <- 1 until K) {
      dp(0) = dp(0) + Math.pow(dLen, i).toInt
    }
    dp(0)
  }

  def main(args: Array[String]): Unit = {
    val D = Array("1", "3", "5", "7")
    val N = 100
    println(atMostNGivenDigitSet(D, N))
  }
}

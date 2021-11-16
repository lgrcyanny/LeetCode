package com.learning.algorithm.dp2.linear.single.dimension

object EggDrop {

  def superEggDrop(K: Int, n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1, K + 1)
    for (k <- 0 to K) {
      dp(0)(k) = 0
      dp(1)(k) = 1
    }
    for (i <- 1 to n) {
      dp(i)(1) = i
    }
    for (i <- 2 to n) {
      for (k <- 2 to K) {
        var left = 1
        var right = i
        while (left + 1 < right) {
          val x = (left + right) / 2
          val t1 = dp(x - 1)(k - 1)
          val t2 = dp(i - x)(k)
          if (t1 < t2) {
            left = x
          } else if (t1 > t2) {
            right = x
          } else {
            left = x
            right = x
          }
        }
        dp(i)(k) = 1 + Math.min(
          Math.max(dp(left - 1)(k - 1), dp(i - left)(k)),
          Math.max(dp(right - 1)(k - 1), dp(i - right)(k))
        )
      }
    }
    println(s"=======dp=======")
    print(dp)
    dp(n)(K)
  }

  /**
   * this version is wrong
   */
  def superEggDropV1(K: Int, n: Int): Int = {
    val dp = Array.ofDim[Int](n + 1, K + 1)
    for (k <- 0 to K) {
      dp(0)(k) = 0
      dp(1)(k) = 1
    }
    for (i <- 1 to n) {
      dp(i)(1) = i
    }
    for (i <- 3 to n) {
      for (k <- 2 to K) {
        dp(i)(k) = Int.MaxValue
        for (f <- 2 to i) {
          dp(i)(k) = Math.min(Math.max(dp(f - 1)(k - 1), dp(i - f)(k)), dp(i)(k)) + 1
        }
      }
    }
    println(s"=======dp=======")
    print(dp)
    dp(n)(K)
  }

  def superEggDropV3(K: Int, n: Int): Int = {
    if (n == 1) {
      1
    } else {
      val dp = Array.ofDim[Int](n + 1, K + 1)
      for (k <- 1 to K) {
        dp(1)(k) = 1
      }
      var res = -1
      for (i <- 2 to n) {
        for (k <- 1 to K) {
          dp(i)(k) = 1 + dp(i - 1)(k - 1) + dp(i - 1)(k)
        }
        if (res < 0 && dp(i)(K) >= n) {
          res = i
        }
      }
      res
    }
  }

  def print(dp: Array[Array[Int]]): Unit = {
    for (i <- 0 until dp.size) {
      println(dp(i).mkString(", "))
    }
  }

  def main(args: Array[String]): Unit = {
    println(superEggDrop(2, 6))
//    println(superEggDropV1(2, 6))
    println(superEggDropV3(2, 100))
  }

}

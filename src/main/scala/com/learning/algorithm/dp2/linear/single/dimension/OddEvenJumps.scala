package com.learning.algorithm.dp2.linear.single.dimension

import java.util
import scala.collection.immutable.TreeMap

object OddEvenJumps {

  def oddEvenJumps(nums: Array[Int]): Int = {
    val N = nums.size
    val K = 2 // k == 0 is even, k == 1 is odd
    val dp = Array.ofDim[Boolean](N, K)
    for (k <- 0 until K) {
      dp(N - 1)(k) = true
    }
    for (i <- N - 2 to 0 by -1) {
      for (k <- 0 until K) {
        dp(i)(k) = false
        k match {
          case 1 => // odd jump
            var jumpIndex = -1
            var jumpValue = Int.MaxValue
            for (j <- (i + 1) until N) {
              if (nums(i) <= nums(j) && nums(j) < jumpValue) {
                jumpIndex = j
                jumpValue = nums(j)
              }
            }
            if (jumpIndex > 0) {
              dp(i)(k) = dp(jumpIndex)(0)
            }
          case 0 => // event jump
            var jumpIndex = -1
            var jumpValue = Int.MinValue
            for (j <- (i + 1) until N) {
              if (nums(i) >= nums(j) && nums(j) > jumpValue) {
                jumpIndex = j
                jumpValue = nums(j)
              }
            }
            if (jumpIndex > 0) {
              dp(i)(k) = dp(jumpIndex)(1)
            }
        }
      }
    }
    val count = dp.filter(_(1)).size
    count
  }

  def oddEvenJumpsOpt(nums: Array[Int]): Int = {
    val N = nums.size
    val index = new util.TreeMap[Int, Int]()
    val odd = Array.ofDim[Boolean](N)
    val even = Array.ofDim[Boolean](N)
    odd(N - 1) = true
    even(N - 1) = true
    index.put(nums(N - 1), N - 1)
    for (i <- N - 2 to 0 by -1) {
      val v = nums(i)
      if (index.containsKey(v)) {
        odd(i) = even(index.get(v))
        even(i) = odd(index.get(v))
      } else {
        try {
          val higher = index.higherKey(v)
          odd(i) = even(index.get(higher))
        } catch {
          case e: NullPointerException =>
        }
        try {
          val lower = index.lowerKey(v)
          even(i) = odd(index.get(lower))
        } catch {
          case e: NullPointerException =>
        }
      }
      index.put(v, i)
    }
    val count = odd.filter(_ == true).size
    count
  }

  def main(args: Array[String]): Unit = {
//    val nums = Array(10, 13, 12, 14, 15) // expect 2
//    val nums = Array(2, 3, 1, 1, 4) // expect 3
//    val nums = Array(5, 1, 3, 4, 2) // expect 3
    val nums = Array(1,2,3,2,1,4,4,5)
    println(oddEvenJumpsOpt(nums))
  }

}

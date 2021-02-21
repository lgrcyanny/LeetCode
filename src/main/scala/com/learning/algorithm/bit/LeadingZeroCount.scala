package com.learning.algorithm.bit

object LeadingZeroCount {

  def leadingZeroForIntCount(n: Int): Int = {
    var count: Int = 0
    var x = n
    for (i <- 1 to 32) {
      if ((x ^ 0) == 0) {
        count = count + 1
      } else {
        count = 0
      }
      x = x >>> 1
    }
    count
  }

  def main(args: Array[String]): Unit = {
    println(s"leading bits: ${Integer.numberOfLeadingZeros(32)}")
    println(leadingZeroForIntCount(32))
  }

}

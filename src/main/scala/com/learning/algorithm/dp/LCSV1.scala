package com.learning.algorithm.dp

import scala.collection.mutable.ArrayBuffer

object LCSV1 {

  def computeLCS(xs: Array[String], ys: Array[String]): Array[String] = {
    // 1. define variables
    val m = xs.length
    val n = ys.length
    // c[i, j] is the length of LCS for xi and yi
    val c: Array[Array[Int]] = Array.ofDim[Int](m + 1, n + 1)
    // b[i, j] is the flag for matched element
    object LCSType extends Enumeration {
      type LCSType = Value
      val UNKNOWN, LEFT, UP, MATCHED = Value
    }
    import LCSType._
    val b = Array.ofDim[LCSType](m + 1, n + 1)
    // 2. do initialize
    for (i <- 0 to m) {
      for (j <- 0 to n) {
        c(i)(j) = 0
        b(i)(j) = UNKNOWN
      }
    }
    // 3. compute LCS c matrix
    for (i <- 1 to m) {
      for (j <- 1 to n) {
        if (xs(i - 1) == ys(j - 1)) {
          c(i)(j) = c(i - 1)(j - 1) + 1
          b(i)(j) = MATCHED
        } else if (c(i - 1)(j) >= c(i)(j - 1)) {
          c(i)(j) = c(i - 1)(j)
          b(i)(j) = UP
        } else {
          c(i)(j) = c(i)(j - 1)
          b(i)(j) = LEFT
        }
      }
    }

    // 4. finalize the LCS Seq
    val result = new ArrayBuffer[String]()
    var i = m
    var j = n
    while (i > 0 && j > 0) {
      if (b(i)(j) == MATCHED) {
        result += xs(i - 1)
        i = i - 1
        j = j - 1
      } else if (b(i)(j) == LEFT) (
        j = j - 1
      ) else if (b(i)(j) == UP) {
        i = i - 1
      }
    }
    // don't forget do reverse here, since we start to do print LCS from position b(m, n)
    result.toArray.reverse
  }

  def testCase1() = {
    val xs: Array[String] = "ABCBDAB".split("")
    val ys: Array[String] = "BDCABA".split("")
    val lcs = computeLCS(xs, ys)
    println(s"lsc is ${lcs.mkString("")}")
  }

  def main(args: Array[String]): Unit = {
    testCase1()
  }
}

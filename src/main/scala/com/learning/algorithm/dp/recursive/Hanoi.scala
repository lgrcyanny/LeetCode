package com.learning.algorithm.dp.recursive

object Hanoi {

  def doMoveHanoi(n: Int, s: String, d: String, e: String): Unit = {
    if (n > 0) {
      doMoveHanoi(n - 1, s, e, d)
      println(s"Move disk ${n} from ${s} to ${d}")
      doMoveHanoi(n - 1, e, d, s)
    }
  }

  def main(args: Array[String]): Unit = {
    doMoveHanoi(3, "source", "destination", "extra")
  }

}

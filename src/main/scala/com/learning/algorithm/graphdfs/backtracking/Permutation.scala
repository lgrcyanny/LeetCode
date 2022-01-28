package com.learning.algorithm.graphdfs.backtracking

import scala.collection.mutable.ArrayBuffer

object Permutation {

  def permute(nums: Array[Int]): List[List[Int]] = {
    val n = nums.size
    val res = new ArrayBuffer[List[Int]]()
    val path = new ArrayBuffer[Int]()
    def _tryPermute(): Unit = {
      if (path.size == n) {
        res.append(path.toList)
      }
      for (i <- 0 until n if path.find(_ == nums(i)).isEmpty) {
        path.append(nums(i))
        _tryPermute()
        path.remove(path.size - 1)
      }
    }
    _tryPermute()
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val res = permute(Array(1, 2, 3))
    res.foreach(l => println(l.mkString(", ")))
  }

}

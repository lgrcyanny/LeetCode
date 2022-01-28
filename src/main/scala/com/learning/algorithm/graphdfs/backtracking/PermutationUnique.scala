package com.learning.algorithm.graphdfs.backtracking

import scala.collection.mutable.ArrayBuffer

object PermutationUnique {

  def permuteUnique(nums: Array[Int]): List[List[Int]] = {
    val n = nums.size
    val sortNums = nums.sorted
    val visited = Array.ofDim[Boolean](n)
    val path = new ArrayBuffer[Int]()
    val res = new ArrayBuffer[List[Int]]()
    def _dfs(index: Int): Unit = {
      if (index == n) {
        res.append(path.toList)
      } else {
        for (i <- 0 until n if !visited(i)) {
          val cutCondition = (i > 0) && (sortNums(i) == sortNums(i - 1)) && (!visited(i - 1))
          if (!cutCondition) {
            path.append(sortNums(i))
            visited(i) = true
            _dfs(index + 1)
            visited(i) = false
            path.remove(path.size - 1)
          }
        }
      }
    }
    _dfs(0)
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 1, 2)
    val res = permuteUnique(nums)
    res.foreach(l => println(l.mkString(", ")))
  }


}

package com.learning.algorithm.graphdfs.backtracking

import scala.collection.mutable.ArrayBuffer

object CombinationSum {

  def combinationSum(candidates: Array[Int], target: Int): List[List[Int]] = {
    val n = candidates.size
    val res = new ArrayBuffer[List[Int]]()
    val path = new ArrayBuffer[Int]()
    def _dfs(index: Int, target: Int): Unit = {
      if (target == 0) {
        res.append(path.toList)
      } else if (target > 0) {
        for (i <- index until n) {
          path.append(candidates(i))
          _dfs(i, target - candidates(i))
          path.remove(path.size - 1)
        }
      }
    }
    _dfs(0, target)
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val candidates = Array(2, 3, 6, 7)
    val solutions = combinationSum(candidates, 7)
    solutions.foreach(l => println(l.mkString(", ")))
  }

}

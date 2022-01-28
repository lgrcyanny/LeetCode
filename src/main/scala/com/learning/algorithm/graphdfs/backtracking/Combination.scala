package com.learning.algorithm.graphdfs.backtracking

import scala.collection.mutable.ArrayBuffer

object Combination {

  def combine(n: Int, k: Int): List[List[Int]] = {
    val res = new ArrayBuffer[List[Int]]()
    val path = new ArrayBuffer[Int]()
    def _dfs(begin: Int, k: Int): Unit = {
      if (k == 0) {
        res.append(path.toList)
      } else if (n - begin + 1 >= k) {
        for (i <- begin to n) {
          path.append(i)
          _dfs(i + 1, k - 1)
          path.remove(path.size - 1)
        }
      }
    }
    _dfs(1, k)
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val solutions = combine(4, 2)
    solutions.foreach(l => println(l.mkString(", ")))
  }

}

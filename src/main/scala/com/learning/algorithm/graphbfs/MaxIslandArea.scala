package com.learning.algorithm.graphbfs

import scala.collection.mutable

object MaxIslandArea {
  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid(0).length
    val directions = Array((-1, 0), (0, -1), (0, 1), (1, 0))
    val visited = Array.ofDim[Boolean](m, n)
    def _isInArea(i: Int, j: Int): Boolean = (i >= 0 && i < m) && (j >= 0 && j < n)
    def _bfs(i: Int, j: Int): Int = {
      val queue = new mutable.Queue[(Int, Int)]()
      queue.enqueue((i, j))
      visited(i)(j) = true
      var res = 0
      while (queue.nonEmpty) {
        val cord = queue.dequeue()
        res = res + 1
        for (direction <- directions) {
          val nextX = direction._1 + cord._1
          val nextY = direction._2 + cord._2
          if (_isInArea(nextX, nextY) && grid(nextX)(nextY) == 1 && !visited(nextX)(nextY)) {
            queue.enqueue((nextX, nextY))
            visited(nextX)(nextY) = true
          }
        }
      }
      res
    }
    var res = 0
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (grid(i)(j) == 1 && !visited(i)(j)) {
          res = Math.max(res, _bfs(i, j))
        }
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(
      Array(0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
      Array(0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
      Array(0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0),
      Array(0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0),
      Array(0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0)
    )
    println(maxAreaOfIsland(grid))
  }

}

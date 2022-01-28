package com.learning.algorithm.graphdfs.backtracking

object MaxIslandArea {

  def maxAreaOfIsland(grid: Array[Array[Int]]): Int = {
    val m = grid.length
    val n = grid(0).length
    val directions = Array((-1, 0), (0, -1), (0, 1), (1, 0))
    val visited = Array.ofDim[Boolean](m, n)
    def _isInArea(i: Int, j: Int): Boolean = (i >= 0 && i < m) && (j >= 0 && j < n)
    def _dfs(i: Int, j: Int): Int = {
      visited(i)(j) = true
      var res = 1
      for (direction <- directions) {
        val nextX = direction._1 + i
        val nextY = direction._2 + j
        if (_isInArea(nextX, nextY) && grid(nextX)(nextY) == 1 && !visited(nextX)(nextY)) {
          res = res + _dfs(nextX, nextY)
        }
      }
      // visited(i)(j) = false, doesn't need, because of max area
      res
    }
    var res = 0
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        if (grid(i)(j) == 1 && !visited(i)(j)) {
          res = Math.max(res, _dfs(i, j))
        }
      }
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val grid = Array(
      Array(0,0,1,0,0,0,0,1,0,0,0,0,0),
      Array(0,0,0,0,0,0,0,1,1,1,0,0,0),
      Array(0,1,1,0,1,0,0,0,0,0,0,0,0),
      Array(0,1,0,0,1,1,0,0,1,0,1,0,0),
      Array(0,1,0,0,1,1,0,0,1,1,1,0,0),
      Array(0,0,0,0,0,0,0,0,0,0,1,0,0),
      Array(0,0,0,0,0,0,0,1,1,1,0,0,0),
      Array(0,0,0,0,0,0,0,1,1,0,0,0,0)
    )
    println(maxAreaOfIsland(grid))
  }

}

package com.learning.algorithm.graphdfs.backtracking

import scala.collection.mutable.ArrayBuffer

object NQueens {

  def genBoard(path: ArrayBuffer[Int]): List[String] = {
    val res = new ArrayBuffer[String]()
    for (col <- path) {
      val s = new StringBuilder
      for (i <- 0 until path.size) {
        if (i == col) {
          s.append("Q")
        } else {
          s.append(".")
        }
      }
      res.append(s.toString())
    }
    res.toList
  }

  def solveNQueens(n: Int): List[List[String]] = {
    val colPlaced = Array.ofDim[Boolean](n)
    val mainDiagonalPlaced = Array.ofDim[Boolean](2 * n - 1)
    val subDiagonalPlaced = Array.ofDim[Boolean](2 * n - 1)
    val path = new ArrayBuffer[Int]()
    val res = new ArrayBuffer[List[String]]()
    def _tryPlace(row: Int, path: ArrayBuffer[Int]): Unit = {
      if (row == n) {
        res.append(genBoard(path))
      } else {
        for (j <- 0 until n) {
          if (!colPlaced(j) && !mainDiagonalPlaced(row - j + n - 1) && !subDiagonalPlaced(row + j)) {
            path.append(j)
            colPlaced(j) = true
            mainDiagonalPlaced(row - j + n - 1) = true
            subDiagonalPlaced(row + j) = true
            _tryPlace(row + 1, path)
            // backtracking
            path.remove(path.size - 1)
            colPlaced(j) = false
            mainDiagonalPlaced(row - j + n - 1) = false
            subDiagonalPlaced(row + j) = false
          }
        }
      }
    }
    _tryPlace(0, path)
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val boards = solveNQueens(4)
    println(boards.size)
    boards.foreach{board =>
      println("==========")
      board.foreach(println)
    }
  }

}

package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.{TreeNode, makeBinaryTree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object LevelOrder {

  def levelOrder(root: TreeNode): List[List[Int]] = {
    if (root == null) {
      Nil
    } else {
      val queue = new mutable.Queue[TreeNode]()
      queue.enqueue(root)
      val res = new ArrayBuffer[ArrayBuffer[Int]]()
      while (queue.nonEmpty) {
        val levelSize = queue.size
        res.append(new ArrayBuffer[Int]())
        for (i <- 0 until levelSize) {
          val front = queue.dequeue()
          res.last.append(front.value)
          if (front.left != null) queue.enqueue(front.left)
          if (front.right != null) queue.enqueue(front.right)
        }
      }
      res.map(_.toList).toList
    }
  }

  def main(args: Array[String]): Unit = {
    /**
     * 5
     * 3       7
     * 2   4   6    8
     */
    val root = makeBinaryTree(List(5, 3, 7, 2, 4, 6, 8))
    val res = levelOrder(root)
    println(res.map(_.mkString("[", ", ", "]")).mkString("[", ",", "]"))
  }

}

package com.learning.leetcode.top100.tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Utils {

   class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
     var value: Int = _value
     var left: TreeNode = _left
     var right: TreeNode = _right
   }

  def makeBinaryTree(values: List[Int]): TreeNode = {
    def insert(root: TreeNode, value: Int): TreeNode = {
      if (root == null) {
        new TreeNode(value)
      } else {
        if (value < root.value) {
          root.left = insert(root.left, value)
        } else {
          root.right = insert(root.right, value)
        }
        root
      }
    }
    values.foldLeft(null: TreeNode)((acc, v) => insert(acc, v))
  }

  def tranverseTree(root: TreeNode): Unit = {
    if (root != null) {
      val queue = new mutable.Queue[TreeNode]()
      val res = new ArrayBuffer[Int]()
      queue.enqueue(root)
      while (!queue.isEmpty) {
        val element = queue.dequeue()
        res.append(element.value)
        if (element.left != null) {
          queue.enqueue(element.left)
        }
        if (element.right != null) {
          queue.enqueue(element.right)
        }
      }
      println(res.mkString(", "))
    }
  }

}

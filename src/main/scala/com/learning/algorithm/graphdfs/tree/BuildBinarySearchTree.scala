package com.learning.algorithm.graphdfs.tree

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object BuildBinarySearchTree {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def bstFromPreorder(preorder: Array[Int]): TreeNode = {
    def _dfs(left: Int, right: Int): TreeNode = {
      if (left > right) {
        null
      } else if (left == right) {
        new TreeNode(preorder(left))
      } else {
        val rootVal = preorder(left)
        val root = new TreeNode(rootVal)
        // binary search the first value less than preorder(left)
        var l = left
        var r = right
        while (l < r) {
          val mid = (l + r + 1) / 2 // if (l + r) / 2, will get endless loop
          if (preorder(mid) < rootVal) {
            l = mid
          } else {
            r = mid - 1
          }
        }
        root.left = _dfs(left + 1, l)
        root.right = _dfs(l + 1, right)
        root
      }
    }
    _dfs(0, preorder.size - 1)
  }

  def bstFromPreorderOpt(preorder: Array[Int]): TreeNode = {
    if (preorder.size == 0) {
      null
    } else {
      val stack = new mutable.ArrayStack[TreeNode]()
      val root = new TreeNode(preorder(0))
      stack.push(root)
      for (i <- 1 until preorder.size) {
        val currentNode = new TreeNode(preorder(i))
        var node = stack.top
        while (stack.nonEmpty && currentNode.value > stack.top.value) {
          node = stack.pop()
        }
        if (currentNode.value < node.value) {
          node.left = currentNode
        } else {
          node.right = currentNode
        }
        stack.push(currentNode)
      }
      root
    }
  }

  def visitTree(root: TreeNode): List[Int] = {
    val res = new ArrayBuffer[Int]()
    def _recur(root: TreeNode): Unit = {
      if (root != null) {
        _recur(root.left)
        res.append(root.value)
        _recur(root.right)
      }
    }
    _recur(root)
    res.toList
  }

  def main(args: Array[String]): Unit = {
    val preorder = Array(8, 5, 1, 7, 10, 12)
    val tree = bstFromPreorderOpt(preorder)
    println(visitTree(tree).mkString(", "))
  }


}

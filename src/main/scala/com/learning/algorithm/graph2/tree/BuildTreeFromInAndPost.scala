package com.learning.algorithm.graph2.tree

import scala.collection.mutable.ArrayBuffer

object BuildTreeFromInAndPost {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  import scala.collection.mutable
  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
    val n = inorder.size
    val inOrderRoot = new mutable.HashMap[Int, Int]()
    for (i <- 0 until inorder.size) {
      inOrderRoot.put(inorder(i), i)
    }
    def _buildRecur(inLeft: Int, inRight: Int, postLeft: Int, postRight: Int): TreeNode = {
      if (postLeft > postRight) {
        null
      } else {
        val rootVal = postorder(postRight)
        val inOrderRootPos = inOrderRoot(rootVal)
        val leftSubTreeSize = inOrderRootPos - inLeft
        val rootNode = new TreeNode(rootVal)
        rootNode.left = _buildRecur(inLeft, inOrderRootPos - 1, postLeft, postLeft + leftSubTreeSize - 1)
        rootNode.right = _buildRecur(inOrderRootPos + 1, inRight, postLeft + leftSubTreeSize, postRight - 1)
        rootNode
      }
    }
    _buildRecur(0, n - 1, 0, n - 1)
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
    val inorder = Array(9, 3, 15, 20, 7)
    val postorder = Array(9, 15, 7, 20, 3)
    val root = buildTree(inorder, postorder)
    println(visitTree(root).mkString(", "))
  }

}

package com.learning.algorithm.graph2.tree

import com.learning.algorithm.graph2.tree.BuildTreeFromInAndPost.TreeNode

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object BuildTreeFromPreAndIn {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  import scala.collection.mutable
  def buildTree(preorder: Array[Int], inorder: Array[Int]): TreeNode = {
    val n = preorder.size
    val inOrderRoot = new mutable.HashMap[Int, Int]()
    for (i <- 0 until inorder.size) {
      inOrderRoot.put(inorder(i), i)
    }
    def _buildRecur(preLeft: Int, preRight: Int, inLeft: Int, inRight: Int): TreeNode = {
      if (preLeft > preRight) {
        null
      } else {
        val rootVal = preorder(preLeft)
        val inOrderRootPos = inOrderRoot(rootVal)
        val leftSubTreeSize = inOrderRootPos - inLeft
        val rootNode = new TreeNode(rootVal)
        rootNode.left = _buildRecur(preLeft + 1, preLeft + leftSubTreeSize, inLeft, inOrderRootPos - 1)
        rootNode.right = _buildRecur(preLeft + leftSubTreeSize + 1, preRight, inOrderRootPos + 1, inRight)
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
    val preorder = Array(3, 9, 20, 15, 7)
    val inorder = Array(9, 3, 15, 20, 7)
    val root = buildTree(preorder, inorder)
    println(visitTree(root).mkString(", "))
  }

}

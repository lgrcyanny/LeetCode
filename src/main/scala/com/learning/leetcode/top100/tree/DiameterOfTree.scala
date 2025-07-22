package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.{TreeNode, makeBinaryTree, tranverseTree}

object DiameterOfTree {
  /**
   * 给你一棵二叉树的根节点，返回该树的直径
   * 二叉树的 直径 是指树中任意两个节点之间最长路径的 长度 。这条路径可能经过也可能不经过根节点 root 。
   * 两节点之间路径的长度 由它们之间边数表示。
   */
  def diameterOfBinaryTree(root: TreeNode): Int = {
    var maxDiameter: Int = 1
    def _depth(root: TreeNode): Int = {
      if (root == null) {
        0
      } else {
        val leftDepth = _depth(root.left)
        val rightDepth = _depth(root.right)
        maxDiameter = Math.max(maxDiameter, leftDepth + rightDepth + 1)
        Math.max(leftDepth, rightDepth) + 1
      }
    }
    _depth(root)
    maxDiameter - 1
  }

  def main(args: Array[String]): Unit = {
    /**
     *        5
     *    3       7
     *  2   4   6    8
     */
    val root = makeBinaryTree(List(5, 3, 7, 2, 4, 6, 8))
    val diameter = diameterOfBinaryTree(root)
    println(diameter)
  }

}

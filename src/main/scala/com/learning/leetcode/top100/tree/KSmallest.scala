package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.TreeNode

import scala.collection.mutable.ArrayBuffer
import java.util

object KSmallest {

  /**
   * 给定一个二叉搜索树的根节点 root ，和一个整数 k ，请你设计一个算法查找其中第 k 小的元素（从 1 开始计数）。
   * @param root
   * @param k
   * @return
   */
  def kthSmallest(root: TreeNode, k: Int): Int = {
    val res = new ArrayBuffer[Int]()
    val stack = new util.Stack[TreeNode]()
    var _current: TreeNode = root
    while (res.size < k && (_current != null || !stack.empty())) {
      while (_current != null) {
        stack.push(_current)
        _current = _current.left
      }
      _current = stack.pop()
      res.append(_current.value)
      _current = _current.right
    }
    res.last
  }

  def main(args: Array[String]): Unit = {
    val tree = Utils.makeBinaryTree(List(6, 5, 7, 3, 4, 8, 1, 2))
    println(Utils.tranverseTree(tree))
    val ksmall = kthSmallest(tree, 3)
    println(s"ksmall k = ${ksmall}, value = ${ksmall}")
  }

}

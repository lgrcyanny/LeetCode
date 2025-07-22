package com.learning.leetcode.top100.tree

import com.learning.leetcode.top100.tree.Utils.{TreeNode, makeBinaryTree}

object BinarySearchTree {

  /**
   * 左子树所有节点值小于root，右子树所有节点值大于root
   * 思路一：递归
   */
  def isValidBST(root: TreeNode): Boolean = {
    def _check(root: TreeNode, lower: Long, upper: Long): Boolean = {
      if (root == null) {
        true
      } else {
        if (root.value <= lower || root.value >= upper) {
          false
        } else {
          _check(root.left, lower, root.value) && _check(root.right, root.value, upper)
        }
      }
    }
    _check(root, Long.MinValue, Long.MaxValue)
  }

  /**
   * 思路二：中序遍历，当前节点值大于前序节点
   * @param args
   */
  def isValidBSTV2(root: TreeNode): Boolean = {
    val stack = new java.util.Stack[TreeNode]()
    var preNodeValue: Long = Long.MinValue
    var _root: TreeNode = root
    var res: Boolean = true
    while ((!stack.isEmpty || _root != null) && res) {
      while (_root != null) {
        stack.push(_root)
        _root = _root.left
      }
      val current = stack.pop()
      if (current.value <= preNodeValue) {
        res = false
      }
      preNodeValue = current.value
      _root = current.right
    }
    res
  }

  def main(args: Array[String]): Unit = {
    val root = makeBinaryTree(List(5, 3, 7, 2, 4, 6, 8))
    val res = isValidBSTV2(root)
    println(res)
  }


}

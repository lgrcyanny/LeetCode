package com.learning.algorithm.graphdfs.app

object SumNumbers {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  def sumNumbers(root: TreeNode): Int = {
    def _recur(node: TreeNode, cumsum: Int): Int = {
      if (node == null) {
        0
      } else if (node.left == null && node.right == null) {
        cumsum * 10 + node.value
      } else {
        val nextSum = cumsum * 10 + node.value
        _recur(node.left, nextSum) + _recur(node.right, nextSum)
      }
    }
    _recur(root, 0)
  }

}

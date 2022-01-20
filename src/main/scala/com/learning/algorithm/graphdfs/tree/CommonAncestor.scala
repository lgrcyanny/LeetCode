package com.learning.algorithm.graphdfs.tree

object CommonAncestor {
  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  import scala.collection.mutable
  def lowestCommonAncestor(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    type Path = List[TreeNode]
    val stack = new mutable.ArrayStack[Path]()
    var pathForP: Path = Nil
    var pathForQ: Path = Nil
    stack.push(root :: Nil)
    while (stack.nonEmpty && (pathForP.isEmpty || pathForQ.isEmpty)) {
      val currentPath = stack.pop()
      if (currentPath.last.value == p.value) {
        pathForP = currentPath
      }
      if (currentPath.last.value == q.value) {
        pathForQ = currentPath
      }
      if (currentPath.last.left != null) {
        stack.push(currentPath :+ currentPath.last.left)
      }
      if (currentPath.last.right != null) {
        stack.push(currentPath :+ currentPath.last.right)
      }
    }
    var i = 0
    var j = 0
    var commonAncestor: TreeNode = null
    var isBreak = false
    while (i < pathForP.size && j < pathForQ.size && !isBreak) {
      if (pathForP(i).value == pathForQ(j).value) {
        commonAncestor = pathForP(i)
        i = i + 1
        j = j + 1
      } else {
        isBreak = true
      }
    }
    commonAncestor
  }

  def lowestCommonAncestorRecur(root: TreeNode, p: TreeNode, q: TreeNode): TreeNode = {
    var ans: TreeNode = null
    def _dfs(root: TreeNode, p: TreeNode, q: TreeNode): Boolean = {
      if (root == null) {
        false
      } else {
        val left = _dfs(root.left, p, q)
        val right = _dfs(root.right, p, q)
        val cond1 = left && right
        val cond2 = (p.value == root.value || q.value == root.value) && (left || right)
        if (cond1 || cond2) {
          ans = root
        }
        left || right || (p.value == root.value || q.value == root.value)
      }
    }
    _dfs(root, p, q)
    ans
  }

}

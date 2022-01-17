package com.learning.algorithm.graph2.tree.traversal

object InOrder {

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
    var value = _value
    var left = _left
    var right = _right
  }

  import scala.collection.mutable
  import scala.collection.mutable.ArrayBuffer
  def inorderTraversal(root: TreeNode): List[Int] = {
    if (root == null) {
      Nil
    } else {
      object TraversalAction extends Enumeration {
        type TraversalAction = Value
        val AddToResult, Go = Value
      }
      import TraversalAction._
      val res = new ArrayBuffer[Int]()
      val stack = new mutable.ArrayStack[(TreeNode, TraversalAction)]()
      stack.push((root, Go))
      while (stack.nonEmpty) {
        val (node, action) = stack.pop()
        if (action == AddToResult) {
          res.append(node.value)
        } else {
          if (node.right != null)  {
            stack.push((node.right, Go))
          }
          stack.push((node, AddToResult))
          if (node.left != null)  {
            stack.push((node.left, Go))
          }
        }
      }
      res.toList
    }
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1, null, new TreeNode(2, new TreeNode(3)))
    val res = preorderTraversal(root)
    println(res.mkString(", "))
  }
}

package com.learning.algorithm.graphdfs.tree.traversal

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PreOrderForN {

  class Node(_value: Int = 0) {
    var value = _value
    var children: List[Node] = Nil
  }

  def preorder(root: Node): List[Int] = {
    if (root == null) {
      Nil
    } else {
      object TraversalAction extends Enumeration {
        type TraversalAction = Value
        val AddToResult, Go = Value
      }
      import TraversalAction._
      val res = new ArrayBuffer[Int]()
      val stack = new mutable.ArrayStack[(Node, TraversalAction)]()
      stack.push((root, Go))
      while (stack.nonEmpty) {
        val (node, action) = stack.pop()
        if (action == AddToResult) {
          res.append(node.value)
        } else {
          for (i <- node.children.size - 1 to 0 by -1) {
            stack.push((node.children(i), Go))
          }
          stack.push((node, AddToResult))
        }
      }
      res.toList
    }
  }

}

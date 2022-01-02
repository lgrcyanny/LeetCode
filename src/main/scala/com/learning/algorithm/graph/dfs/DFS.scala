package com.learning.algorithm.graph.dfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DFS {

  case class Node(id: Int, children: List[Node] = List.empty) {
    override def hashCode(): Int = id

    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case n: Node => n.id == this.id
        case _ => false
      }
    }
    override def toString: String = s"Node$id"
  }

  def dfs(node: Node): List[Node] = {
    val visited = new mutable.HashSet[Node]()

    def dfsVisit(node: Node): List[Node] = {
      if (visited.contains(node)) {
        println(s"already visited: ${node}")
        Nil
      } else {
        visited.add(node)
        val childNodes = (for (child <- node.children) yield dfsVisit(child)).flatten
        node :: childNodes
      }
    }

    dfsVisit(node)
  }

  def dfsNonRecursive(root: Node): List[Node] = {
    val waitingToVisit = new mutable.ArrayStack[Node]()
    val visited = new mutable.HashSet[Node]()
    val result = new ArrayBuffer[Node]()
    waitingToVisit.push(root)
    while (waitingToVisit.nonEmpty) {
      val node = waitingToVisit.pop()
      if (!visited.contains(node)) {
        println(s"Visit ${node}")
        visited.add(node)
        for (child <- node.children) {
          waitingToVisit.push(child)
        }
        result.append(node)
      }
    }
    result.toList
  }

  def main(args: Array[String]): Unit = {
    val node5 = Node(5)
    val node3 = Node(3, List(node5))
    val node4 = Node(4, List(node5))
    val node2 = Node(2, List(node3, node4))
    val root = Node(1, List(node2))
    val res = dfs(root)
    println(s"res size: ${res.size}, ${res.mkString("[", ",", "]")}")
  }

}

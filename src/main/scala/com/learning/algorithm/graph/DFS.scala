package com.learning.algorithm.graph

object DFS {

  case class Node(id: Int, children: List[Node] = List.empty) {
    override def hashCode(): Int = id

    override def equals(obj: scala.Any): Boolean = obj match {
      case n: Node => n.id == this.id
      case _ => false
    }

    override def toString: String = s"Node$id"
  }

  def dfs(node: Node): List[Node] = {
    def dfsIter(node: Node, visited: Set[Node]): List[Node] = {
        if (visited.contains(node)) {
          Nil
        } else {
          val childNodes = (for (child <- node.children) yield dfsIter(child, visited + node)).flatten
          node :: childNodes
        }
    }
    dfsIter(node, Set())
  }

  def main(args: Array[String]): Unit = {
    val node3 = Node(3)
    val node4 = Node(4)
    val node2 = Node(2, List(node3, node4))
    val root = Node(1, List(node2))
    val res = dfs(root)
    println(s"res size: ${res.size}, ${res.mkString("[", ",", "]")}")
  }

}

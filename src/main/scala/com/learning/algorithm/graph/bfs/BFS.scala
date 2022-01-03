package com.learning.algorithm.graph.bfs

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object BFS {

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

  def bfs(node: Node): List[Node] = {
    def bfsIter(
      visited: mutable.Set[Node],
      pendingQueue: mutable.Queue[Node],
      result: mutable.Queue[Node]): mutable.Queue[Node] = {
      if (pendingQueue.nonEmpty) {
        val front = pendingQueue.dequeue()
        println(s"Visit ${front}")
        result.enqueue(front)
        for (child <- front.children) {
          if (!visited.contains(child)) {
            pendingQueue.enqueue(child)
            visited.add(child)
          } else {
            println(s"Already visited ${front}")
          }
        }
        bfsIter(visited, pendingQueue, result)
      } else {
        result
      }
    }

    val visited = new mutable.HashSet[Node]
    val pendingQueue = new mutable.Queue[Node]()
    val result = new mutable.Queue[Node]()
    pendingQueue.enqueue(node)
    bfsIter(visited, pendingQueue, result)
    result.toList
  }

  def bfsNonRecursive(root: Node): List[Node] = {
    val waitingToVisit = new mutable.Queue[Node]()
    val visited = new mutable.HashSet[Node]()
    val result = new ArrayBuffer[Node]()
    waitingToVisit.enqueue(root)
    while (waitingToVisit.nonEmpty) {
      val node = waitingToVisit.dequeue()
      println(s"Visit ${node}")
      result += node
      for (child <- node.children) {
        if (!visited.contains(child)) {
          visited.add(child)
          waitingToVisit.enqueue(child)
        } else {
          println(s"Already visited ${node}")
        }
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
    val res = bfsNonRecursive(root)
    println(s"res size: ${res.size}, ${res.mkString("[", ",", "]")}")
  }

}

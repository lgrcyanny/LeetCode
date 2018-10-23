package com.learning.algorithm.linklist

object SingleListReverse {

  case class Node(data: Int, var next: Option[Node] = None)

  def reverse(root: Option[Node]): Option[Node] = {
    var p: Option[Node] = None
    var q: Option[Node] = root // current
    var r: Option[Node] = if (q.nonEmpty) q.get.next else None

    while (q.nonEmpty) {
      q.get.next = p
      p = q
      q = r
      r = if (r.nonEmpty) r.get.next else r
    }
    p
  }

  def print(root: Option[Node]): Unit = {
    if (root.nonEmpty) {
      println(root.get.data)
      print(root.get.next)
    } else {
      println("None")
    }
  }

  def main(args: Array[String]): Unit = {
    val n1 = Node(1)
    val n2 = Node(2)
    val n3 = Node(3)
    val n4 = Node(4)
    n1.next = Some(n2)
    n2.next = Some(n3)
    n3.next = Some(n4)
    println("original:")
    print(Some(n1))
    val reversed = reverse(Some(n1))
    println("reversed:")
    print(reversed)
  }

}

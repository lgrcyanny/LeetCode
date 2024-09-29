package com.learning.leetcode.top100.linkedlist

object Utils {

  class ListNode(var _x: Int = 0) {
    var next: ListNode = null
    var x: Int = _x
  }


  def makeList(nums: Array[Int]): ListNode = {
    if (nums.isEmpty) {
      null
    } else {
      val n = new ListNode(nums.head)
      n.next = makeList(nums.tail)
      n
    }
  }

  def tranverse(head: ListNode): Unit = {
    println("=======")
    var p = head
    while (p != null) {
      println(p.x)
      p = p.next
    }
  }

}

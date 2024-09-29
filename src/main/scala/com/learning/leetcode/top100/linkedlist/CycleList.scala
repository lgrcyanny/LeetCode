package com.learning.leetcode.top100.linkedlist

object CycleList {
  import com.learning.leetcode.top100.linkedlist.Utils._

  def hasCycle(head: ListNode): Boolean = {
    if (head == null) {
      false
    } else {
      var p = head
      var q = head.next
      var isCycle = false
      while (p != null && q != null && q.next != null && !isCycle) {
        if (p == q) {
          isCycle = true
        } else {
          p = p.next
          q = q.next.next
        }
      }
      isCycle
    }
  }

  def detectCycle(head: ListNode): ListNode = {
    if (head == null) {
      null
    } else {
      var slow = head
      var fast = head
      var cycleEntryNode: ListNode = null
      while (fast != null && fast.next != null && cycleEntryNode == null) {
        slow = slow.next
        fast = fast.next.next
        if (slow == fast) {
          var ptr = head
          while (ptr != slow) {
            ptr = ptr.next
            slow = slow.next
          }
          cycleEntryNode = ptr
        }
      }
      cycleEntryNode
    }
  }


}

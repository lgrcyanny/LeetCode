package com.learning.leetcode.top100.linkedlist

import com.learning.leetcode.top100.linkedlist.Utils._

object IntersectionNode {

  /**
   * 给你两个单链表的头节点 headA 和 headB ，请你找出并返回两个单链表相交的起始节点。如果两个链表不存在相交节点，返回 null
   * 思路: 相交的点到末尾的长度一样, 让两个指针从与末尾同等距离的地方开始遍历，该位置是短链表的头结点。需要消除长度差异
   * 过程: 1.pa, pb两个节点依次往后遍历
   *      2.pa如果为null，则pa指向headB, pb如果为null, 则pb指向headA
   *      3.继续遍历，如果相等就相交
   */
  def getIntersectionNode(headA: ListNode, headB: ListNode): ListNode = {
    if (headA == null || headB == null) {
      null
    } else {
      var pA = headA
      var pB = headB
      while (pA != pB) {
        pA = if (pA == null) headB else pA.next
        pB = if (pB == null) headA else pB.next
      }
      pA
    }
  }

}

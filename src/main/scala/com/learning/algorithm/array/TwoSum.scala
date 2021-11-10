package com.learning.algorithm.array

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object TwoSum {

  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    val n = nums.size
    val index = new mutable.HashMap[Int, ArrayBuffer[Int]]
    for (i <- 0 until n) {
      val v = nums(i)
      if (index.get(v).isEmpty) {
        index.put(v, new ArrayBuffer[Int]())
        index(v).append(i)
      } else {
        index(v).append(i)
      }
    }
    val res = new ArrayBuffer[Int]()
    for (i <- 0 until n) {
      val left = target - nums(i)
      if (res.isEmpty && index.get(left).isDefined && index(left).filter(_ != i).nonEmpty) {
        res.append(i)
        val leftIndex = index(left).filter(_ != i).head
        res.append(leftIndex)
      }
    }
    res.toArray
  }

  def twoSumOpt(nums: Array[Int], target: Int): Array[Int] = {
    val n = nums.size
    val index = new mutable.HashMap[Int, Int]
    val res = new ArrayBuffer[Int]()
    for (i <- 0 until n) {
      val left = target - nums(i)
      if (index.contains(left)) {
        res.append(index(left))
        res.append(i)
      }
      index.put(nums(i), i)
    }
    res.toArray
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(3, 2, 4)
    println(twoSum(nums, 6).mkString(", "))
    println(twoSumOpt(nums, 6).mkString(", "))
  }

}

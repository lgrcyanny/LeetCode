package com.learning.algorithm.array

import scala.collection.mutable

object FindLeftRepeatNum {

  def search(nums: Array[Int]): Array[Int] = {
    if (nums.isEmpty) {
      Array.empty[Int]
    } else {
      val res = Array.ofDim[Int](nums.length)
      val hash = new mutable.HashMap[Int, Int]()
      for (i <- 0 until nums.length) {
        if (!hash.contains(nums(i))) {
          res(i) = -1
          hash.put(nums(i), i)
        } else {
          res(i) = hash(nums(i))
        }
      }
      res
    }
  }

  def search2(nums: Array[Int]): Array[Int] = {
    if (nums.isEmpty) {
      Array.empty[Int]
    } else {
      val res = Array.ofDim[Int](nums.length)
      val hash = new mutable.HashMap[Int, Int]()
      for (i <- 0 until nums.length) {
        if (!hash.contains(nums(i))) {
          res(i) = -1
          hash.put(nums(i), i)
        } else {
          res(i) = hash(nums(i))
          hash.put(nums(i), i)
        }
      }
      res
    }
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 3, 1, 2, 1)
    val res = search(nums)
    println(res.mkString(", "))
  }

}

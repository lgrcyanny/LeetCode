package com.learning.algorithm.dp

object StringInterleavingPrinter {

  def getInterleavingString(a: String, b: String): Array[String] = {
    def concat(c: Char, strs: Array[String]): Array[String] = strs.map(s => s"${c}${s}")
    def recur(m: Int, n: Int): Array[String] = {
      if (m == a.size && n == b.size) {
        Array.empty[String]
      } else if (m == a.size) {
        Array(b.slice(n, b.size))
      } else if (n == b.size) {
        Array(a.slice(m, a.size))
      } else {
        val first = concat(a(m), recur(m + 1, n))
        val second = concat(b(n), recur(m, n + 1))
        first ++ second
      }
    }
    recur(0, 0)
  }

  def main(args: Array[String]): Unit = {
    val a = "AB"
    val b = "XY"
    val result = getInterleavingString(a, b)
    println(result.mkString(", "))
  }

}

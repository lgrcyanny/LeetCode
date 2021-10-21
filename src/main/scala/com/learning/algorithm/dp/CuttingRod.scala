package com.learning.algorithm.dp

object CuttingRod {

  /**
   * maxValue(len) = max(currentMaxValue, price(len) + maxValue(len - i)), for i in lengths
   */
  def maxCuttingValue(length: Int, prices: Map[Int, Int]): (Int, Seq[Int]) = {
    def _recur(totalLen: Int, totalValue: Int, sequences: Seq[Int]): (Int, Seq[Int]) = {
      if (totalLen <= 0) {
        (totalValue, sequences)
      } else {
        val solutions = for ((len, price) <- prices.filter(_._1 <= totalLen))
          yield _recur(totalLen - len, totalValue + price,  sequences :+ len)
        val (maxValue, maxSeq) = solutions.maxBy(_._1)
        (maxValue, maxSeq)
      }
    }
    _recur(length, 0, Seq.empty[Int])
  }

  case class CutSolution(value: Int = 0, seq: Seq[Int] = Seq.empty) {
    def addRod(len: Int, price: Int): CutSolution = {
      CutSolution(value + price, seq :+ len)
    }
  }

  def maxCuttingValueDP(length: Int, prices: Map[Int, Int]): (Int, Seq[Int]) = {
    val memo = Array.ofDim[CutSolution](length + 1) // memo[i] is the max value for length i
    for (i <- 0 to length) {
      memo(i) = CutSolution()
    }
    for (totalLen <- 1 to length) {
      for ((len, value) <- prices) {
        if (len <= totalLen && (value + memo(totalLen - len).value > memo(totalLen).value)) {
          memo(totalLen) = memo(totalLen - len).addRod(len, value)
        }
      }
    }
    (memo(length).value, memo(length).seq)
  }

  def main(args: Array[String]): Unit = {
    val prices = Map(1 -> 1, 2 -> 5, 3 -> 8, 4 -> 9, 5 -> 10, 6 -> 17, 7->17, 8 -> 20)
    val l = 8
    val (v, s) = maxCuttingValueDP(l, prices)
    println(s"rod length ${l}, max value ${v}, seq: ${s.mkString("[", ",", "]")}")
  }

}

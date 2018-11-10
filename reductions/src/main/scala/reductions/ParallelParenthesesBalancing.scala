package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 10000
    val chars = new Array[Char](length)
    val threshold = 100
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def balanceHelper(chars: Array[Char], count: Int): Boolean = {
      if (chars.isEmpty) count == 0
      else {
        if (chars.head == '(') balanceHelper(chars.tail, count + 1)
        else if (chars.head == ')') balanceHelper(chars.tail, count - 1) && count > 0
        else balanceHelper(chars.tail, count)
      }
    }
    balanceHelper(chars, 0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int): (Int, Int) = {
      var left, right = 0
      var index = idx
      while (index < until) {
        chars(index) match {
          case '(' => left += 1
          case ')' => {
            if (left > 0) left -= 1
            else right += 1
          }
          case _ =>
        }
        index += 1
      }
      (right, left)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val size = until - from
      if (size < threshold) traverse(0, chars.length)
      else {
        val mid = from + (until - from) / 2
        val ((right1, left1), (right2, left2)) = parallel(reduce(from, mid), reduce(mid, until))
        if (left1 > right2) (right1, left1 - right2 + left2)
        else (left1 + right1 - right2, left2)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}

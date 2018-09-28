package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceHelper(chars: List[Char], count: Int): Boolean = {
        if (chars.isEmpty) count == 0
        else
        if (chars.head == '(') balanceHelper(chars.tail, count + 1)
        else if (chars.head == ')') count > 0 && balanceHelper(chars.tail, count - 1)
        else balanceHelper(chars.tail, count)
      }
      balanceHelper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countingHelper(money: Int, coins: List[Int]) : Int = {
        if (money == 0) 1
        else if (coins.isEmpty || money < 0) 0
        else
          countingHelper(money - coins.head, coins) + countingHelper(money, coins.tail)
      }
      if (money == 0) 0 else countingHelper(money, coins)
    }
  }

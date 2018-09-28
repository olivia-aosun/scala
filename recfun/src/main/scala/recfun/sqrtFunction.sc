object Exercise {
  def countChange(money: Int, coins: List[Int]): Int = {
    def countingHelper(money: Int, coins: List[Int]) : Int = {
      if (money == 0) 1
      else if (coins.isEmpty || money < 0) 0
      else
        countingHelper(money - coins.head, coins) + countingHelper(money, coins.tail)
    }
    if (money == 0) 0 else countingHelper(money, coins)
  }
  val list = List(1, 2, 4)
  countChange(4, list)
}
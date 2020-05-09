package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0 || c < 0  || c > r)
      0
    else if (r == 0)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @scala.annotation.tailrec
    def getInnerParenthesisCount(innerChars: List[Char], parenthesisLevel: Int): Int = {
      if (innerChars.isEmpty)
        parenthesisLevel
      else {
        val deltaLevel = innerChars.head match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
        val nextParenthesisLevel = parenthesisLevel + deltaLevel
        if (nextParenthesisLevel < 0)
          nextParenthesisLevel
        else
          getInnerParenthesisCount(innerChars.slice(1, innerChars.length), nextParenthesisLevel)
      }
    }
    val unbalance = getInnerParenthesisCount(chars, 0)
    unbalance == 0
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else {
      val acceptableSortedCoins = coins.filter(coinValue => coinValue <= money).distinct.sorted.reverse
      if (acceptableSortedCoins.isEmpty)
        0
      else
        acceptableSortedCoins.map(coinValue => countChange(
          money - coinValue,
          acceptableSortedCoins.filter(newCoinsExpected => newCoinsExpected <= coinValue))
        ).sum
    }
  }
}

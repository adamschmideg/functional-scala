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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || r == c) 1
      else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def almostBalance(chars: List[Char], closingParenCount: Int): Boolean = {
        if (chars.isEmpty) closingParenCount == 0
        else if (closingParenCount < 0) false
        else if (chars.head == '(') almostBalance(chars.tail, closingParenCount + 1)
        else if (chars.head == ')') almostBalance(chars.tail, closingParenCount - 1)
        else almostBalance(chars.tail, closingParenCount)
      }

      almostBalance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) 0
      else if (money <= 0) 0
      else if (money == coins.head) 1 + countChange(money, coins.tail)
      else if (coins.length == 1) if (money % coins.head == 0) 1 else 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }

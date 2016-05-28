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
      
      if(c == 0 || r == 1) 1 else pascal(c - 1, r) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def getWeight(c: Char): Int = {
        val weight = Map( 
          '(' -> 1, 
          ')' -> -1 
        )
        if(weight isDefinedAt c) weight(c) else 0 
      }
      
      def balanceIter(chars: List[Char], n: Integer): Boolean = {
        if(chars.isEmpty) n == 0
        else if(n<0) false
        else balanceIter(chars.tail, n + getWeight(chars.head))
      }

      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }

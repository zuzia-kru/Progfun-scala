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
    def pascal(c: Int, r: Int): Int = if (c==0 || c==r) 1 else pascal(c-1,r-1) + pascal(c,r-1)

  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

    def balanceIter(chars: List[Char], x: Int): Boolean = {
      if (chars.isEmpty && x == 0) true
      else {
        if (chars.isEmpty || x<0) false else balanceIter(chars.tail, updateIter(chars.head, x))
      }
    }
    def updateIter(head: Char, x: Int): Int = if (head=='(') x+1 else if (head==')') x-1 else x

    balanceIter(chars,0)
  }

  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = ???
  }

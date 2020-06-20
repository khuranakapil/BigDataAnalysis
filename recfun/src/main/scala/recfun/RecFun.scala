package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    //println(balance(":-)".toList))
    //val coins = List(1, 2, 3, 4)
    //println(countChange(4, coins))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    {
      def emptycase(stack: Int): Boolean = if (stack > 0) false else true
      def checkparan(stack: Int, chars: List[Char]): Boolean = if (chars.isEmpty) emptycase(stack) else if (chars.head == '(') checkparan(stack+1, chars.tail) else if (chars.head == ')' && stack == 0) false else if(chars.head == ')') checkparan(stack-1, chars.tail) else checkparan(stack, chars.tail)
      checkparan(0, chars)
    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    {
      if (money == 0) 1 else if (coins.isEmpty) 0 else if (money < coins.head) countChange(money, coins.tail) else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
}

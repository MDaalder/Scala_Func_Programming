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
    if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)

    }

  //println(pascal(1,2))
  //println("Did I do it?")

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def listIter(chars: List[Char], check: Int): Boolean = {
      //println(chars, check)
      if (check < 0) false
      else if (chars.isEmpty && check != 0) false
      else if (chars.isEmpty) check == 0

      else if (chars.head == '(') listIter(chars.tail, check + 1)
      else if (chars.head == ')') listIter(chars.tail, check - 1)
      else listIter(chars.tail, check)
    }

    listIter(chars, 0)
  }

  //println(balance("(if (zero? x) max (/ 1 x)) is balanced".toList))

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countIter(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else countIter(money, coins.tail) + countIter(money - coins.head, coins)
    }
    countIter(money, coins)
  }
}

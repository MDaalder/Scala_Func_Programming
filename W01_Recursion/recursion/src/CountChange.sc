
  def countChange(money: Int, coins: List[Int]): Int = {
    // need to see how many times the denominations from coin list can fit to make money #
    // going to need division, operands
    // other scenarios?
    // all possible options with coins[0], coins[1], coins[2] etc
      // so coins[0] % money = 0

    def remainder(moneyRem: Int, coinsRem: List[Int], count: Int): Int ={
      // use to keep track of coins and money before iterating down them?

      def coinIterate(moneyLeft: Int, coinsLeft: List[Int], count: Int): Int = {
        // start by computing cases that use one coin to get moneyLeft to zero
        if (moneyLeft % coinsLeft.head == 0) coinIterate(money, coinsLeft.tail, count + 1)
        else if (coinsLeft.tail.nonEmpty) coinIterate(money, coinsLeft.tail, count)
        else remainder(money, coinsRem.tail, count)

      def
      // we divide by one coin to get moneyLeft, the try the next coin until 0
        // then we start the process over with the next coin
        if (moneyLeft % coinsLeft.head > 0) remainder(moneyLeft % coinsLeft.head, coinsLeft.tail, count)
        else remainder(money, coinsLeft.tail, count + 1)

        // now we subtract a single coin, one by one, from the money pile
        if (moneyLeft - coinsLeft.head > 0) remainder(moneyLeft - coinsLeft.head, coinsLeft.tail, count)
        else remainder(money, coinsLeft.tail, count + 1)
      }
      // then start with 2nd coin, go back to first
      // then start with 3rd coin, go back to first, then second, etc

      // can we swap coins with money somehow like in the gcd example?

      // search for the remainder in the coins we have i.e. if we have divided by 12 and the remainder is 3, find if there is a 3




      if (moneyLeft == 0) remainder(money, coins)

      if (coinsLeft.isEmpty) remainder(money, coins)

    }
    coins.isEmpty
    coins.head
    coins.tail


  }

  countChange(4, List(1,2,))
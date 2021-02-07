object exercise{

  def balance(chars: List[Char]): Boolean = {
    def listIter(chars: List[Char], closeCheck: List[Char]): Boolean = {
      //if (chars.isEmpty) true
      if (chars.head == '(') {
        if (closeCheck.head == ")".toList) listIter(chars.tail, closeCheck.tail)
        else listIter(chars, closeCheck.tail)
      }
      else listIter(chars.tail, closeCheck.tail)
    }

    listIter(chars, chars.tail)
  }

  balance("if (zero? x) max (/ 1 x)) is balanced".toList)

}

// iterate through the list of chars and parse out when a char is a parenthesis
// can we simply remove all other characters, while keeping the sequence of parentheses in a separate val?
// then test that each parenthesis closes
// Sublists and element access:

//  xs.length   the number of elements of xs
//  xs.last     the list's last element
//  xs.init     a list consisting of all elements of xs except the last one
//  xs take n   a list consisting of the first n elements of xs, or xs if it is shorter than n
//  xs drop n   the rest of the collection after taking n elements
//  xs(n)       the element of xs at index n

// creating new lists:

//  xs ++ ys    the listing of all elements of xs followed by all elements of ys
//  xs.reverse  the list containing all elements of xs in reversed order
//  xs updated (n, x)   the list containing all of the same elements as xs, except at index n where it contains x

// Finding elements:

//  xs indexOf x    the index of the first element in xs equal to x, or -1 if x does not appear in xs
//  xs contains x   same as xs indexOf x >= 0

// returns the last element of a list
// complexity is O(n) : has to traverse the list one element at a time
def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

// init returns all elements of the list except the last one
def init[T](xs: List[T]): List[T] = xs match{
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

// concat returns the lists xs and ys merged together, with elements of xs being the new first elements (xs ::: ys)
// complexity is O(n) corresponding the the length of xs, i.e. O(n) -> |xs|
def concat[T](xs: List[T], ys: List[T]): List[T] = xs match{
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

// reverses the given list
// complexity is linear in size of xs for concatenation ( ++ List(y) part), with additional
//    linear traversal of xs for the recursive call of ys
// complexity is O(n*n) because we traverse xs twice, once for the recursive call and once for the concatenation of the lists
def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}


// returns all elements of the list except for the element at nth index
// tried to do from first principles. Don't know if it works or not.
// the easy method is = (xs take n) ::: (xs drop n + 1)
def removeAtEasy[T](xs: List[T], n: Int): List[T] = (xs take n) ::: (xs drop n + 1)

def removeAt[T](xs: List[T], n: Int): List[T] = {

    def iterator[T](xs: List[T], n: Int, ansList: List[T]): List[T] = xs match {
    case List() => List()
    case y :: ys => {
      if (n > 0) iterator(ys, n - 1, ansList ::: List(y))
      else if (n == 0) ansList ::: ys
      else throw new Error("index out of range")
    }
  }
  iterator(xs, n, List())
}

removeAt(List('a', 'b', 'c', 'd'), 1)
removeAtEasy(List('a', 'b', 'c', 'd'), 1)
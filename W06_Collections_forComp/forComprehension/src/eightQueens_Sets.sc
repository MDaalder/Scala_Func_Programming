
// sets are another basic abstraction in the Scala collections
val fruit = Set("apple", "banana", "pear")
val s = (1 to 6).toSet
// most operations on sequences are also available on sets
s map (_ + 2) // Set(3, 4, 5, 6, 7, 8)
//fruit filter (_.startsWith == "app") // Set("apple")
s.nonEmpty // true

// sets and sequences have some fundamental differences
// 1. Sets are unordered: the elements of a set do not have a predefined order in which they appear in the set
// 2. Sets do not have duplicate elements
s map (_ / 2) // Set(2, 0, 3, 1) ; the original set was 6 elements, now it's 4 because duplicates were removed. there's also no order.
// 3. the fundamental operation on sets is contains
s contains 5 // true

/**
 * Eight Queens Problem
 * Place eight queens on a chessboard so that no queen is threatened by the other
 *
 * One way to solve this, place a queen in each row
 * Once have placed k-1 queens, one must place the kth queen in a column where it's not threatened by any other queen on the board
 *
 * The Solution can be had with a recursive algorithm
 *
 * Suppose we have already generated all the solutions consisting of placing k-1 queens on a board of size n
 * Each soln is represented by a list (of length k-1) containing the number of columns (between 0 and n-1)
 * The column num of the queen in the k-1th row comes first in the list, followed by the column number the queen in row k-2, etc
 * The solution set is thus represented as a set of lists, with one element for each soln
 * Now, to place the kth queen, we generate all possible extensions of each soln preceded by a new queen
 */

// place n queens on a chess board of n rows


import scala.math.abs


// n is number of rows on the chess board
def queens(n: Int): Set[List[Int]] = {

  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens // map (x => (queens(_), _)) // pairs of (col, row)

    queensWithRow forall {
      case (r, c) => col != c && abs(col - c) != row - r
    }
  }

  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  placeQueens(n)

}

def show(queens: List[Int]): String = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString + "\n"
     (lines mkString "\n")
}

(queens(8) take 3 map show).mkString "\n" // soln for n=8 queens (queens(n))
val test2 = Set(List(0, 0, 0, 1), List(1, 0, 0, 0))

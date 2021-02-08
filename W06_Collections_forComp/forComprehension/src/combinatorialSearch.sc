def isPrime(n: Int): Boolean = {
  (2 until n) forall (n % _ != 0)
}
val n = 7

// generate the sequence of all pairs of integers (i, j), where 1 <= j < i < n
// then filter the pairs for which i + j is prime

// this can generate the sequence of all pairs as a Vector(Vector(), Vector(), ...)
val xss = (1 until n) map (i =>
  (1 until i) map (j => (i, j)))

// however, we want a list of pairs, not a collection of vector of vectors
// the previous code gave a sequence of sequences ( call it xss )
// we can combine all the sub-sequences using foldRight with ++:

// (xss foldRight Seq[Int]())(_ ++ _)
// or, we use the built-in method flatten
xss.flatten
// equivalent to
((1 until n) map (i =>
  (1 until i) map (j => (i, j)))).flatten

// useful law
// xs flatMap f = (xs map f).flatten
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j)))

// to find pairs whose sum are prime numbers
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter (pair =>
  isPrime(pair._1 + pair._2))

case class Person(name: String, age: Int)

val p1 = Person("bob", 21)
val p2 = Person("carol", 20)
val p3 = Person("jude", 25)

val persons: List[Person] = List(p1, p2, p3)

// to obtain the names of persons over 20 years old:
for ( p <- persons if p.age > 20) yield p.name
// this is equivalent to
persons filter (p => p.age > 20) map (p => p.name)

// a for-expression is of the form
// for ( s ) yield e
// where is a sequence of generators and filters, and e is an expression whose value is returned by an iteration

// a generator is of the form p <- e (p range over all e), where p is a pattern and e an expression whose value is a collection
// a filter is of the form if f where f is a boolean expression
// ( s ) can be written as { s } so a sequence of generators and filters can be written on multiple lines without ;

for {
  i <- 1 until n // range from 1 until n
  j <- 1 until i // range from 1 until i
  if isPrime(i + j)
} yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]): Double =
  (for ((x, y) <- xs zip ys) yield (x * y)).sum

//// to compute the scalar product of two vectors
//def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
//  (xs zip ys).map(xy => xy._1 * xy._2).sum
//// alternative pattern matching function
//def scalarProductMatch(xs: Vector[Double], ys: Vector[Double]): Double =
//  (xs zip ys).map{ case (x, y) => x * y}.sum

val test1 = List(1, 2, 3, 44): List[Double]
val test2 = List(22, 3, 2, 1): List[Double]

scalarProduct(test1, test2)

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
queens(4)

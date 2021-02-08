
// vectors are arrays of 32 elements, that once all 32 elements are full
// each element points to another 32 elements
// vector arrays grow at 2^(5*d) where d is the depth of the arrays
// so the first vector is 2^5 elements (32), the second layer is 2^10 = 1024 elements, etc

// vectors are created analogously to lists
// they support the same operations as lists, with the exception of ::
// use +: adds new element to less of "list", :+ adds an element to the right of the "list"
// adding to a vector is of complexity O(log32(N))

// vectors and lists have a common base class Seq, the class of a all sequences. This is a subclass of Iterable
// arrays and strings are also of class Seq

// range is a sequence Seq of evenly spaced integers
// range has three operators: to (inclusive), until (exclusive), by (to determine step value)
// ranges are represented as single objects with three fields: lower bound, upper bound, step value

val nums = Vector(1, 2, 3, -88)
val people = Vector("Bob", "James", "Peter")

val xs = Vector(1, 2, 3, 44)
xs map (x => x * 2)

val s = ("Hello World")
s filter (c => c.isUpper)
s exists (c => c.isUpper) // true
s forall (c => c.isUpper) // false

val r: Range = 1 until 5 // 1, 2, 3, 4
val t: Range = 1 to 5 // 1, 2, 3, 4, 5
1 to 10 by 3 // 1, 4, 7, 10
6 to 1 by -2 // 6, 4, 2


//  xs exists p     true if there is an element x of xs such that p(x) holds, false otherwise
//  xs forall p     true if p(x) holds for all elements x of xs, false otherwise
//  xs zip ys       A sequence of pairs drawn from corresponding elements of sequences xs and ys
//  xs.unzip        splits a sequence of pairs xs into two sequences consisting of the first, respectively second havles of all pairs
//  xs.flatMap f    applies collection-valued function f to all elements of xs and concatenates the results
//  xs.sum          the sum of all elements of this numeric collection
//  xs.product
//  xs.max          the maximum of all elements of this collection (an Ordering must exist)
//  xs.min          the minimum of all elements of this collection

val pairs = List(1, 2, 3) zip s // combines the two Seq as pair values List[(Int, Char)] in this case
pairs.unzip

s flatMap (c => List('.', c))

xs.sum
xs.max

// example
// To list all combinations of numbers x and y where x is drawn from 1..M and y is drawn from 1..N
//val M = 5
//val N = 6
//(1 to M) flatMap (x => 1 to N)map(y => (x, y))

// to compute the scalar product of two vectors
def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map(xy => xy._1 * xy._2).sum
// alternative pattern matching function
def scalarProductmatch(xs: Vector[Double], ys: Vector[Double]): Double =
  (xs zip ys).map{ case (x, y) => x * y}.sum

val test1 = Vector(1, 2, 3, 44): Vector[Double]
val test2 = Vector(22, 3, 2, 1): Vector[Double]
scalarProduct(test1, test2) // ((1, 22), (2, 3), (3, 2), (44, 1)) => (22*1) + (2*3) + (3*2) + (44*1) = 78.0

// example to determine if a number is prime (not efficient)
def isPrime(n: Int): Boolean = {
  (2 until n) forall (d => n % d != 0)
}




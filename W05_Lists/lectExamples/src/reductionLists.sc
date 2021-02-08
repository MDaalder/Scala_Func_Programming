
// applications of foldLeft and reduceLeft unfold on trees that lean to the left
// they have two dual functions, foldRight and reduceRight, which produce trees which lean to the right

//  reduceLeft    inserts a given binary operator between adjacent elements of a list. Can only be applied to nonEmpty lists
//                List(x1, ..., xn) reduceLeft op = (...(x1 op x2) op ...) op xn
//  foldLeft      is like reducedLeft but takes an accumulator as an additional parameter
//                which is returned when foldLeft is called on an empty list
//                (List(x1, ..., xn) foldLeft z)(op) = (...(z op x1) op ...) op xn
//
//  reduceRight
//                List(x1, ..., x{n-1}, xn) reduceRight op = x1 op ( ... (x{n-1} op xn) ... )
//
//                (List(x1, ..., xn) foldRight acc)(op) = x1 op ( ... (xn op acc) ... )
//

// sum(List(x1, ..., xn)) = 0 + x1 + ... + xn

def sum(xs: List[Int]): Int = xs match {
  case Nil => 0
  case y :: ys => y + sum(ys)
}

//using reduceLeft we can simplify

def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft((x, y) => x + y) // start list with a 0
def product(xs: List[Int]): Int = (1 :: xs) reduceLeft((x, y) => x * y) // start list with a 1 to avoid multiply by 0

// ((x, y) => x * y) can be rewritten as (_ * _)
def sum(xs: List[Int]): Int = (0 :: xs) reduceLeft(_ + _) // start list with a 0
def product(xs: List[Int]): Int = (1 :: xs) reduceLeft(_ * _)

// using foldLeft, more general utilization when input lists are empty
def sum(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
def product(xs: List[Int]): Int = (xs foldLeft 1)(_ * _)

//// foldLeft and reduceLeft can be implemented in class List as follows

//abstract class List[T] {...
//  def reduceLeft(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil.reduceLeft")
//    case x :: xs => (xs foldLeft x)(op)
//  }
//  def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
//    case Nil => z
//    case x :: xs => (xs foldLeft op(z, x))(op)
//  }
//
//
//  def reduceRight(op: (T, T) => T): T = this match {
//    case Nil => throw new Error("Nil.reduceRight")
//    case x :: Nil => x
//    case x :: xs => op(x, xs.reduceRight(op))
//  }
//  def foldRight[U](z: U)(op: (T, U) => U): U = this match {
//    case Nil => z
//    case x :: xs => op(x, (xs foldRight z)(op))
//  }
//}

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight ys)(_ :: _)

//// foldLeft will not work here! you get a type error
//// :: is only applicable to lists, not arbitray elements. using foldLeft applies :: to elements rather than lists.
//def concat[T](xs: List[T], ys: List[T]): List[T] =
//  (xs foldLeft ys)(_ :: _)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _) // (x, y) => f(x) :: y, where x is xs.head and y is the next element

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x, y) => 1 + y ) // where x is xs.head and y is the next element

val nums = List(2, -4, 5, 7, 1)

mapFun[Int, String](nums, x => x + "some")
lengthFun(nums)

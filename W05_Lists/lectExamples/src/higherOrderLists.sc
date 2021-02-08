// map, filter

// xs map p           applies the function p to all elements of list xs

// xs filter p        selection of all elements of a list satisfying a given condition p, see example
// xs filterNot p     same as xs filter (x => !p(x)) the list consisting of those elements of xs that do not satisfy the predicate
// xs partition p     same as (xs filter p, xs filterNot p), but computed in a single traversal of the list xs
// xs takeWhile p     the longest prefix of list xs consisting of elements that all satisfy the predicate p
// xs dropWhile p     the remainder of the list xs after any leading elements satisfying p have been removed
// xs span p          same as (xs takeWhile p, xs dropWhile p) but computed in a single traversal of the list xs


// applying a function to elements of a list

// might want to transform each element of a list and then return the list of results

// i.e. might want to multiply each element of a list by the same factor
def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
  case Nil => xs
  case y :: ys => y * factor :: scaleList(ys, factor)
}

// this scheme can be generalized to the method map of the List class
// a simple way to define map is as follows:

//abstract class List[T] { ...
// def map[U](f: T => U) List[U] = this match {
//    case Nil => this
//    case x :: xs => f(x) :: xs.map(f)
//  }

// using map, we write scaleList more concisely to create a list of elements xs multiplied by a factor
def scaleListMap(xs: List[Double], factor: Double) =
  xs map (x => x * factor)

// lecture activity to square all elements of a list
def squareList(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => y*y :: squareList(ys)
}

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x*x)




// selection of all elements satisfying a given condition. I.e. positive Ints
def posElems(xs: List[Int]): List[Int] = xs match {
  case Nil => xs
  case y :: ys => if (y > 0) y :: posElems(ys) else posElems(ys)
}

// this pattern can be generalized by the method filter of the List class

//abstract class List[T] { ...
// def filter(p: T => Boolean): List[T] = this match {
//    case Nil => this
//    case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
//}

// using filter, posElems can be written more concisely
def posElems(xs: List[Int]): List[Int] =
  xs filter (x => x > 0)


val nums = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

nums filter (x => x > 0)
nums filterNot (x => x > 0)
nums partition (x => x > 0) // gives two lists for which predicate is true, false. Traverses list once

nums takeWhile (x => x > 0)
nums dropWhile (x => x > 0)
nums span (x => x > 0)

// write a function that packs consecutive duplicates of list elements into sublists

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    first :: pack(rest)
}

val data = List("a", "a", "a", "b", "c", "c", "a")
pack(data)
// expect List(List(a, a, a), List(b), List(c, c), List(a))

// adds an encoding function to the pack function
def packEncode[T](xs: List[T]): List[(T, Int)] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span (y => y == x)
    (first.head, first.length) :: packEncode(rest)
}

// encodes a list using the pack function and map
// map works because pack has created sublists within a list.
// You can apply a function that work for all elements of each sublist, separately
def encode[T](xs: List[T]): List[(T, Int)] =
  pack(xs) map (ys => (ys.head, ys.length))

packEncode(data)
encode(data)
// expect List(List(a, 3), List(b, 1), List(c, 2), List(a, 1))

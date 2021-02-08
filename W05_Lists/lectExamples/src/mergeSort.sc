// merge sort with only Integers

//// merge sort using pattern matching on a single object.
//// Look ahead for a better merge sort code
//def msort(xs: List[Int]): List[Int] = {
//  val n = xs.length/2
//  if (n == 0) xs
//  else {
//    def merge(xs: List[Int], ys: List[Int]): List[Int] =
//      xs match {
//        case Nil => ys
//        case x :: xs1 =>
//          ys match {
//            case Nil => xs
//            case y :: ys1 => {
//              if (x < y) x :: merge(xs1, ys) // x is smallest and should be first element
//              else y :: merge(xs, ys1) // y is smallest and should be first element
//            }
//          }
//      }
//    val (fst, snd) = xs splitAt n
//    merge(msort(fst), msort(snd))
//  }
//}

// merge sort using pattern matching on a pair. This is better!
def msort(xs: List[Int]): List[Int] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[Int], ys: List[Int]): List[Int] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msort(fst), msort(snd))
  }
}

val nums = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

msort(nums)

// to do merge sort on anything else than Int, we need to parameterize merge with necessary comparison function
// the most flexible design is to make the function sort polymorphic and to pass the comparison operation as an additional parameter
def msortAny[T](xs: List[T])(lessThan: (T, T) => Boolean): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (lessThan(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msortAny(fst)(lessThan), msortAny(snd)(lessThan))
  }
}


msortAny(nums)((x: Int, y: Int) => x < y) // defined the lessThan function to be passed
msortAny(fruits)((x: String, y: String) => x.compareTo(y) < 0) // returns -1 if first string is earlier in alphabet than 2nd string, 0 if equal, +1 if greater


// there is already a class in the standard library that represents orderings

// scala.math.Ordering[T]
// provides ways to compare elements of type T
// we can parameterize with Ordering instead of creating our own lessThan
// by using implicit in front of the ord parameter, we can leave it out when calling the function
import scala.math.Ordering

def msortOrdering[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length/2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if (ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
    }
    val (fst, snd) = xs splitAt n
    merge(msortOrdering(fst), msortOrdering(snd))
  }
}

msortOrdering(nums) // the implicit ordering function will determine the type to sort on its own
msortOrdering(fruits)
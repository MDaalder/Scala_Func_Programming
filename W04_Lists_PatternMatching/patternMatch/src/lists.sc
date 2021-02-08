
val fruit = List("apples", "oranges", "pears")
val diag3 = List(List(1,0,0), List(0,1,0), List(0,0,1))

// all lists are constructed from the empty list Nil and
// the construction operation ::
// x :: xs gives a new list with the first element x, followed by the element xs

// the following is equivalent to the list 'fruit' above
val fruit2 = "apples" :: ("oranges" :: ("pears" :: Nil))

val nums = 1 :: 2 :: 3 :: 4 :: Nil

// all operations on lists can be expressed in terms of the following three operations:
//    head    the first element of the list
//    tail    the list composed of all the elements except the first
//    isEmpty 'true' if the list is empty, 'false' otherwise

fruit.head    == "apples"
fruit.tail.head == "oranges"
diag3         == List(1, 0, 0)
// empty.head == throw new NoSuchElementException("head of empty list")

print(fruit)
print(fruit2)
print(nums)


// can decompose lists with pattern matching
//   Nil  the nil c constants
//   p :: ps  a pattern that matches a list with head matching p and a tail matching ps
//  List(p1, ..., pn)   same as p1 :: ... :: pn :: Nil

// 1 :: 2 :: xs       Lists that start with a 1 and then a 2, rest of list arbitrary and bound to variable xs
// x :: Nil           Lists of length 1, the first element can be arbitrary and is bound to  variable x
// List(x)            Sames as x :: Nil
// List()             The empty list, same as Nil
// List(2 :: xs)      A list that contains as only element another list that starts with 2

// consider the pattern  x :: y :: List(xs, ys) :: zs
// what is the condition that describes most accurately the lenght L of the lists it matches?

// ans: L >= 3
// x, y, List(xs, ys) are the first 3 elements of the list, while zs is a variable representing any list, including Nil

// one way to sort a list by sorting the tail and inserting the head in the right place
// Insertion Sort



// inserts an element x in a list xs that is already sorted
def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: (insert(x, ys)) // y is the smallest element so far, need to insert x into the remaining list ys
}

// this does the Insertion Sort
def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}
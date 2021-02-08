

// a list Cons(x, xs) consists of a head elem x and a tail list xs
// using val head: Int in the function defines the parameters and fields of a class at the same time
//class Cons(val head: Int, val tail: IntList) extends IntList

// an empty list Nil
//class Nil extends IntList


trait List[T] {

  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  def isEmpty = false
  // def head = T is already implemented in the method head
}

class Nil[T] extends List[T]{
  // don't need the types listed explicitly, just put here for show. Nothing is a subtype of any other type
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
}

//// functions can have type parameters
//def singleton[T](elem: T) = new Cons[T](elem, Nil[T])
//
//singleton[Int](1) // has cells {1, .}
//singleton[Boolean](true) // has cells {true, .}

//// the compiler can usually deduce correct type. Can omit type parameters
//singleton(1) // will deduce 1 is a type Int
//singleton(true) // will deduce true is of type Boolean

def nth[T](n: Int, xs: List[T]): T = {
  if (xs.isEmpty) throw new IndexOutOfBoundsException
  else if (n == 0) xs.head
  else nth(n-1, xs.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, new Nil))) // list = (1,2,3)
nth(2, list) // expect 3 as the n=2 element
nth(-1, list)
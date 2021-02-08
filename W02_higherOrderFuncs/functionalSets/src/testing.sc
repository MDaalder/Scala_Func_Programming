import funsets.FunSetsInterface


  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
type FunSet = Int => Boolean


def contains(s: FunSet, elem: Int): Boolean = s(elem)

def singletonSet(elem: Int): FunSet = (x:Int) => x == elem

def union(s: FunSet, t: FunSet): FunSet = (x:Int) => s(x) || t(x)

def intersect(s: FunSet, t: FunSet): FunSet = (x:Int) => s(x) && t(x)

def diff(s: FunSet, t: FunSet): FunSet = (x:Int) => s(x) && !t(x)

def filter(s: FunSet, p: Int => Boolean): FunSet = (x:Int) => s(x) && p(x)

/**
 * The bounds for `forall` and `exists` are +/- 1000.
 */
val bound = 1000

/**
 * Returns whether all bounded integers within `s` satisfy `p`.
 */
def forall(s: FunSet, p: Int => Boolean): Boolean = {
  def iter(a: Int): Boolean = {
    if (a > bound) true
    else if (s(a) && !p(a)) false
    else iter(a+1)
  }
  iter(-bound)
}
/**
 * Returns whether there exists a bounded integer within `s`
 * that satisfies `p`.
 */
def exists(s: FunSet, p: Int => Boolean): Boolean = !forall(s,x => !p(x))

/**
 * Returns a set transformed by applying `f` to each element of `s`.
 */
def map(s: FunSet, f: Int => Int): FunSet = exists(s, y => x == f(y))
 // forall(s, f(x => x) => f(x => x) )
//  (f(x => x)) => forall(s, )
//else if (contains(diff(s, p), a)) false
  // must be Int => Boolean

/**
 * Displays the contents of a set
 */
def toString(s: FunSet): String = {
  val xs = for (i <- -bound to bound if contains(s, i)) yield i
  xs.mkString("{", ",", "}")
}

///**
// * Prints the contents of a set on the console.
// */
//def printSet(s: FunSet): Unit = {
//  println(toString(s))
//}

  val pop:FunSet =  singletonSet(0) // x => x == 2
  val pop2:FunSet = singletonSet(2) // (x:Int) => x == 0
  val pop3:FunSet = singletonSet(-4) // 4(x:Int) => pop(x) || pop2(x)
  val pop4 = union(pop, pop2)
  val pop5 = union(pop3, pop4)

  !forall(pop5, x => !(x == 0))

  contains(pop5, -1)

  pop(0)
  pop2(2)
  pop3(2)

  contains(pop, -2)
  println("Testing")
  pop
object Hello {

  def main(args: Array[String]) = println("Hello World!")
}


object example extends App {

  abstract class IntSet {
    // methods in an abstract class do not need a function body
    def contains(x: Int): Boolean = false

    def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    def union(other: IntSet): IntSet //= this incl other

  }
//  only require one instance of class Empty, so we change it to an object
//  class Empty extends IntSet {
//    override def contains(x: Int): Boolean = false
//    override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//    override def toString = "."
//  }

  // object here defines a singleton object named Empty
  // No other Empty instance can be (or needs to be) created
  // Singleton objects are values, so Empty evaluates to itself
  object Empty extends IntSet {
    override def contains(x: Int): Boolean = false
    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
    def union(other: IntSet): IntSet = other
    override def toString = "."
  }
  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

    override def contains(x: Int): Boolean =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmpty(elem, left incl x, right)
      else if (x > elem) new NonEmpty(elem, left, right incl x)
      else this

    def union(other: IntSet): IntSet =
      ((left union right) union other) incl elem
    // this recursion should terminate because every call to union is on a set that is smaller than the current NonEmpty set
    // at some point we hit zero (Empty) in which case the union is instant and the "other" IntSet is returned

    override def toString = "{" + left + elem + right + "}"
  }

//  val t1 = new IntSet incl 3
//  val t2 = t1 incl 4
  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4

  println(t1, t2)
}
//
//class Empty extends IntSet {
//
//  override def contains(x: Int): Boolean = false
//
//  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//
//  override def toString = "."
//}
//
//class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
//
//  override def contains(x: Int): Boolean =
//    if (x < elem) left contains x
//    else if (x > elem) right contains x
//    else true
//
//  override def incl(x: Int): IntSet =
//    if (x < elem) new NonEmpty(elem, left incl x, right)
//    else if (x > elem) new NonEmpty(elem, left, right incl x)
//    else this
//
//  override def toString = "{" + left + elem + right + "}"
//}



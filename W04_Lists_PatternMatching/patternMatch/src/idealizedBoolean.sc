//package idealized.scala

// creates our own, idealized Boolean class of type object
//abstract class Boolean {
//
//  object true extends Boolean{
//    def ifThenElse[T](t: => T, e: => T) = t
//  }
//
//  object false extends Boolean {
//    def ifThenElse[T](t: => T, e: => T) = e
//  }
//
//  def ifThenElse[T](t: => T, e: => T): T
//
//  def && (x: => Boolean): Boolean = ifThenElse(x, false)
//  def || (x: => Boolean): Boolean = ifThenElse(true, x)
//  def unary_!: Boolean            = ifThenElse(false, true)
//
//  def == (x: Boolean): Boolean    = ifThenElse(x, x.unary_!)
//  def != (x: Boolean): Boolean    = ifThenElse(x.unary_!, x)
//
//  def < (x: Boolean): Boolean     = ifThenElse(false, x)
//
//}

// creating a class of positive natural numbers from first principles
// piano numbers is the technical name for this
abstract class Nat {

  def isZero: Boolean
  def predecessor: Nat
  //def successor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predecessor does not exist")
  //def successor = new Succ(this)
  def + (that: Nat) = that
  def - (that: Nat) = if (that.isZero) this else throw new Error("negative number")
}


class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  //def successor = new Succ(this)  //(n + Zero.successor)
  def + (that: Nat) = new Succ(n + that)
  def - (that: Nat) = if (that.isZero) this else n - that.predecessor
}

// class of type Rational, used for fractions
class Rational(x: Int, y: Int) {
  // test that is performed when the class is initiated, throws IllegalArgumentException
  require(y != 0, "denominator must be nonzero")

  // if a Rational is a whole number, don't need to put in the denominator
  // This is used in the function position, means it is a constructor of the class
  def this(x: Int) = this(x, 1)

  // gcd is greatest common divisor used to simplify Rationals
  // private members can only be accessed from inside the class
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else (gcd(b, a % b))

  // this was commented out to include in the toString method
  // private val g = gcd(x, y)
  def numer = x /// g // can be x / gcd(x, y) to remove the above if numer and denom aren't called often
  def denom = y /// g // can be val = y / gcd(x, y) if we plan to call denom often, so calculate it only once

  // checks if Rational is less than that.Rational
  def < (that: Rational) = numer * that.denom < that.numer * denom // def less(that: Rational) = numer * that.denom < that.numer * denom

  // find maximum of two rationals, this refers to the object on which the method is being executed
  def max(that: Rational) = if (this < that) that else this

  // method makes the rational negative
  def unary_- : Rational = // def neg: Rational =
    new Rational(-numer, denom)

  // creating a data abstraction in a class is called a method
  // this adds two Rationals (fractions) together
  def + (that: Rational) = { //def add(that: Rational) = {
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
  }

  // subtract two Rationsl from each other
  def - (that: Rational) = this + -that // def sub(that: Rational) = add(that.neg)

//  // subtract two Rationals from each other
    // this was removed because the method neg was added, and made the above method more elegant
//  def sub(anon: Rational) = {
//    new Rational(
//      numer * anon.denom - anon.numer * denom,
//      denom * anon.denom)
//  }

  // gets all Rationals to print nicely as an actual fraction
  // performs simplification of Rational only when strings are called, instead of any time the class is called
  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }

}

// made new object x of type Rational
// x.numer and x.denom returns those items from the Rational fraction
val x = new Rational(1,3)
x.numer
x.denom
val y = new Rational(5,7)
val z = new Rational(3,2)

// adds Rationals x and y
x + y // x add y // x.add(y)
y + y // y add y // y.add(y)
// turns Rational x negative
-x //neg x // x.neg
-y
x-y-z
// subtraction of x-y-z
x - y - z // x sub y sub z //(x.sub(y)).sub(z)
// checks if x is less than y
x < y //x.less(y)
// checks for largest fraction
x max y //x.max(y)

new Rational(2)





// function to add two Rationals (two fractions)
// this was abstracted by putting an add method into the class Rational
def addRational(r: Rational, s: Rational): Rational = {
  new Rational(
    r.numer * s.denom + s.numer * r.denom,
    r.denom * s.denom)
  }

// takes rational and produces numerator and denominator as a string
// this was made obsolete by the method toString in class Rational
def makeString(r: Rational) =
  r.numer + "/" + r.denom

// add two Rationals (fractions) and makes a string
makeString(addRational(new Rational(1,2), new Rational(2,3)))

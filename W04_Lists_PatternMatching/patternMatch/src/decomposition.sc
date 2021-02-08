
// class hierarchy:
// Expr with two subclasses Number & Sum

// want to know if a given expression is a subclass Number or Sum
// and what the Int is or what the operands are

// the superClass Expr would have methods for classification and methods for accesors

// writing all these classification and accesor methods becomes tedious
trait Expr {
  // 2 classification methods
  def isNumber: Boolean
  def isSum: Boolean
  // 3 accessor methods for either Numbers or Sums
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}

class Number(n: Int) extends Expr {
  def isNumber: Boolean = true
  def isSum: Boolean = false
  def numValue: Int = n
  def leftOp: Expr = throw new Error("Number.leftOp")
  def rightOp: Expr = throw new Error("Number.rightOp")
}

class Sum(e1: Expr, e2: Expr) extends Expr{
  def isNumber: Boolean = false
  def isSum: Boolean = true
  def numValue: Int = throw new Error("Sum.numValue")
  def leftOp: Expr = e1
  def rightOp: Expr = e2
}

// gives us the option to do eval(Sum(Number(1), Number(2)) that will return 3
def eval(e: Expr): Int = {
  if (e.isNumber) e.numValue
  else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
  else throw new Error("Unknown expression " + e)
}


//// let's add a class for Product and Variable
//// if we continued with access and classification methods, we would need to add new methods to Expr and to every class extending Expr
//// this would require creating 3 new methods in each Expr, Number, Sum (5+3 = 8 methods total, each) and then 8 methods to each new subclass.
//// that is 25 new method lines just to add two new subclasses when only 2 were pre-existing
//// this is a quadratic increase of methods as we add new classes to the hierarchy
//class Prod(e1: Expr, e2: Expr) extends Expr {
//
//}
//
//class Var(x: String) extends Expr {
//
//}


// the following could get rid of classification methods, but is low-level and potentially unsafe
// it uses types tests and type casts (e.isInstanceOf[Type] and e.asInstanceOf[Type])
// type casts might not succeed or throw exceptions
def eval(e: Expr): Int =
  if (e.isInstanceOf[Number])
    e.asInstanceOf[Number].numValue
  else if (e.isInstanceOf[Sum])
    eval (e.asInstanceOf[Sum].leftOp) +
      eval (e.asInstanceOf[Sum].rightOp)
  else throw new Error("Unknown expression " + e)

// if all we want to do is evaluate expressions...
// You could define:

trait Expr2 {
  def eval: Int
}
class Number2(n: Int) extends Expr2{
  def eval: Int = n
}
class Sum2(e1: Expr2, e2: Expr2) extends Expr2 {
  def eval: Int = e1.eval + e2.eval
}

// however, this only allows us to evaluate expressions, it would require adding new methods to every class
// for functions like showing a string, or simplifying expressions


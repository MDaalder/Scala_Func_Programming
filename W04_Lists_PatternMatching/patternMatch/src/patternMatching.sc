// the following is a follow-up to the decomposition.sc file using trait Expr and subclasses Number, Sum, etc.

// the task we are trying to solve is to find a general and convenient way
// to access objects in an extensible class hierarchy
// tried to do this with object oriented decomposition, but it didn't work perfectly

// the sole purpose of test and accessor functions is to ~reverse~ the construction process
// i.e. 'which subclass was used?' 'what were the arguments of the constructor?'
// Many functional languages (Scala) automate this process because it's so common


// the case class is similar to a normal class definition but adds some functionality
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(n: String) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

// the case class implicitly defines companion objects with apply methods
// i.e. object Number { def apply(n: Int) = new Number(n) } and object Sum { def apply(e1: Expr, e2: Expr) = new Sum(e1, e2) }
// so you can write Number(1) instead of new Number(1)

// Match Syntax:
//    match is followed by a sequence of cases, pattern => expression
//    each Case associates an expression with a pattern
//    a MatchError exception is throw if no pattern matches the value of the selector
def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
  case Var(x) => throw new Error("Can't evaluate Var")
  case Prod(e1, e2) => eval(e1) * eval(e2)
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1, e2) => show(e1) + " + " + show(e2)
  case Var(x) => x
//  case Prod(Sum(e1, e2), e2) => "(" + show(e1) + ")" + " * " + show(e2)
  case Prod(Sum(e1, e2), e3) => "(" + show(Sum(e1, e2)) + ")" + " * " + show(e3)
  case Prod(e1, e2) => show(e1) + " * " + show(e2)
}
// patterns are constructed from:
//    constructors, eg Number, Sum
//    variables, eg n, e1, e2
//    wildcard patterns _
//    constants, eg 1, true, "string"
//  The same variable name can only appear once in a pattern, Sum(x, x) is not a legal pattern
//  Variables begin with a lowercase letter
//  Names of constants begin with a capital letter, exceptions are reserved words null, true, false

eval(Sum(Number(1), Number(4)))
show(Sum(Number(1), Number(4)))
show(Prod(Number(1), Var("x")))
show(Sum(Prod(Number(2), Var("x")), Var("y")))
show(Prod(Sum(Number(2), Var("x")), Var("y")))
eval(Prod(Number(4), Var("x")))
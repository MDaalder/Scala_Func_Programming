// fundamental collection type is the map

// a map of type Map[Key, Value] is a data structure that associates keys of type Key with values of type Value
// maps are iterable
// Class Map[Key, Value] also extends the function type Key => Value so maps can be used everywhere functions can

val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10) // Map[String, Int]
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern") // Map[String, String]

capitalOfCountry("US") // "Washington"

// applying a map to a non-existing key gives an error
// capitalOfCountry("andorra") would throw an Error NoSuchElementException

// to query a map without knowing whether it contains a given key, you can use the get operation
// this will return an Option value
capitalOfCountry get "andorra" // get an Option value that reads None
capitalOfCountry get "US" // get an Option value that reads Some(Washington)

//// Option type is defined as
//trait Option[+A]
//case class Some[+A](value: A) extends Option[A]
//object Non extends Option[Nothing]
//// the expression map get key returns
//// None     if map does not contain the given key
//// Some(x)  if map associates the given key with the value x

// can use pattern matching to decompose
def showCapital(country: String) = capitalOfCountry.get(country) match{
  case Some(capital) => capital
  case None => "missing data"
}

showCapital("US") // "washington"
showCapital("Andorra") // "missing data"

// two useful operation o fSQL queries in addition to for-expressions are
// groupBy and orderBy

// orderBy on a collection can be expressed by sortWith and sorted

val fruit = List("apple", "pear", "orange", "pineapple")
fruit sortWith (_.length < _.length) // List("pear", "apple", "orange", "pineapple")
fruit.sorted // List("apple", "orange", "pear", "pineapple")

// groupBy is available on Scala collections. it partitions a collection into a map of collections
// according to a discriminator function f

fruit groupBy (_.head) // >Map(p -> List(pear, pineapple),
                        // |    a -> List(apple),
                        // |    o -> List(orange))

// a polynomial can be seen as a map from exponents to coefficients
// x^3 - 2x + 5 can be expressed with the map
Map(0 -> 5, 1 -> -2, 3 -> 1) // where the Key is the exponent to the x

// this is to add polynomials. It's unwieldy, there's a better way.
class Poly(val terms: Map[Int, Double]) {
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }

  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
}

val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

// there is an operation withDefaultValue that turns a map into a total function
// can set default values for things such as an Option= None value

val cap1 = capitalOfCountry withDefaultValue "<unknown>"
cap1 ("Andorra") // "<unknown>"

// simplified the def adjust function by adding a withDefaultValue 0.0 to make any None coeff1 be = 0.0
class Poly0(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly0) = new Poly0(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp)) // coeff is the 2nd List, terms(exp) is the first list
    }


  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
}

val p10 = new Poly0(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p20 = new Poly0(Map(0 -> 3.0, 3 -> 7.0))
p10 + p20
p10.terms(7)

// added bindings. Applies, in this case, changing inputs to a Map
class Poly1(terms1: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)  // )* indicates it's a sequence
  val terms = terms1 withDefaultValue 0.0
  def + (other: Poly1) = new Poly1(terms ++ (other.terms map adjust))
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp)) // coeff is the 2nd List, terms(exp) is the first list
  }


  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
}

val p11 = new Poly1(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p21 = new Poly1(0 -> 3.0, 3 -> 7.0)
p11 + p21
p11.terms(7)

class PolyFoldLeft(termsFL: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)  // )* indicates it's a sequence
  val terms = termsFL withDefaultValue 0.0
  // foldLeft takes terms as the base value and applies the addTerm function as: terms func other.terms
  def + (other: PolyFoldLeft) = new PolyFoldLeft((other.terms foldLeft terms)(addTerm))
  // def addTerm(0th element = terms, and a term = other.terms
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))
  }


  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted) yield coeff + "x^" + exp) mkString " + "
}

val p1FL = new PolyFoldLeft(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2FL = new PolyFoldLeft(0 -> 3.0, 3 -> 7.0)
p1FL + p2FL
p1FL.terms(7)
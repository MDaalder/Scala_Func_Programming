import patmat.{CodeTree, HuffmanInterface, Leaf}

abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
 * Assignment 4: Huffman coding
 *
 */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Fork(l, r, chars, w1) => w1
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Fork(l, r, chars, w1) => chars
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees
  //val sampleTree = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e', 1)), Leaf('t', 2))

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  //
  //def times(chars: List[Char]): List[(Char, Int)] = {
  //
  //  if (chars.isEmpty) List()
  //
  //  def pairing(chars: List[Char], elem: Char, acc: Int): (Char, Int) = {
  //    if (chars.tail.isEmpty) (elem, acc)
  //    else if (chars.head == chars.tail.head) pairing(chars.tail, elem, acc+1)
  ////    else times(chars.tail)
  //    else pairing(chars.tail, chars.tail.head, 1)
  //  }
  //
  // List(pairing(chars, chars.head, 1))
  //
  //}

  def times(chars: List[Char]): List[(Char, Int)] = {
    /**
     * groupBy: Partitions the traversable collection into a map of traversable collections
     * according to some function
     * chars.groupBy(x => x) : return a map of (char, List(char, char....))
     */
    chars.groupBy(x => x).map(t => (t._1, t._2.length)).iterator.toList
  }

  /**
   * using map as an alternative
   * */
  //  def times2(chars: List[Char]): List[(Char, Int)] = {
  //    def iterate(map: Map[Char, Int], c: Char) = {
  //      val count = (map get c).getOrElse(0) + 1
  //      map + ((c, count))
  //    }
  //    // /: alternative of chars foldLeft Map[Char, Int]()
  //    (Map[Char, Int]() /: chars)(iterate).iterator.toList
  //  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] =  {

//    if (freqs.isEmpty) List()
//    else (isort(freqs))

    def insert(x: (Char, Int), xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List(x)
      //case ys :: Nil => List[(Char, Int)](ys._1, ys._2)
      case y :: ys => if (x._2 <= y._2) x :: xs else y :: insert(x, ys)
    }

    def isort(xs: List[(Char, Int)]): List[(Char, Int)] = xs match {
      case List() => List()
      case y :: ys => insert(y, isort(ys))
    }

    def tupleToLeaf(xs: List[(Char, Int)]): List[Leaf] = xs match {
      case List() => List()
      case y :: ys =>(new Leaf(xs.head._1, xs.head._2)) :: tupleToLeaf(ys)
    }

    tupleToLeaf(isort(freqs))
//    case List() => List()
//    case x :: Nil => List(new Leaf(x._1, x._2))
//    case x :: xs => if (x._2 <= xs.head._2) List((new Leaf(x._1, x._2)) :: makeOrderedLeafList(xs)) else x :: makeOrderedLeafList(xs)
  }

  times(List('a', 'b', 'a', 'c', 'c')) //returns a list of pairs List(('a', 2), ('b', 1))
  // val pair: (Char, Int) = ('c', 1)

  //times2(List('c', 'b', 'a', 'c', 'c'))
}
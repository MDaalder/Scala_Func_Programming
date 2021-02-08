import forcomp.Anagrams.{Occurrences, Sentence, Word, combinations, dictionary, dictionaryByOccurrences, sentenceOccurrences, subtract, wordOccurrences}

List("Every", "student", "likes", "Scala").groupBy((element: String) => element.length)

val words = List("Every", "student", "likes", "eat", "ate", "tea", "silke")

def wordOccurrences(w: Word): Occurrences = {
  val chars = (for (chr <- w) yield chr).toLowerCase
  (((chars groupBy (_.toChar)).map(p => (p._1, p._2.length))).toList).sorted
}

/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = {
  wordOccurrences(s.mkString(""))
}

lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
  //    val words = for (word <- dictionary) yield List(wordOccurrences(word))
  dictionary groupBy wordOccurrences
}

def wordAnagrams(word: Word): List[Word] =
  dictionaryByOccurrences(wordOccurrences(word))

wordAnagrams("ate")

def combinations(occurrences: Occurrences): List[Occurrences] = {
  val ocs: List[Occurrences] = occurrences.map(x => (for (i <- 1 to (x._2)) yield (x._1, i)).toList)
  ocs.foldRight(List[Occurrences](Nil))((x, y) => y ++ (for (i <- x; j <- y) yield (i :: j)))
}

//val ocs: List[Occurrences] = wordOccurrences("ATEE").map(x => (for (i <- 1 to (x._2)) yield (x._1, i)).toList)
//ocs.foldRight(List[Occurrences](Nil))((x, y) => y ++ (for (i <- x; j <- y) yield (i :: j)))

def subtract(x: Occurrences, y: Occurrences): Occurrences = {
  val yMap = y.foldLeft(Map[Char, Int]())((acc, e) => acc + (e._1 -> e._2))
  x.map(occ => (occ._1, occ._2 - yMap.getOrElse(occ._1, 0))).filter(_._2 > 0)
}

val x: Occurrences = List(('a', 1), ('d', 1), ('l', 1), ('r', 3))
val y: Occurrences = List(('r', 1), ('d', 1))

//val z = x.foldLeft((x.head)) ((xx, yy) => (xx._1,  xx._2 - yy._2))

//// finds where y and x have overlap and can be subtracted
//// creates the list of new subtracted elements, but hasn't updated the original x list
//val test =
//  for {
//    (yx, yy) <- y
//    (xx, xy) <- x
//    if (yx == xx && yy <= xy)
//  } yield (yx, xy - yy)//x updated (elem, (yx, xy - yy))
//
/** A word is simply a `String`. */
type Word = String
type Sentence = List[Word]
type Occurrences = List[(Char, Int)]

def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

  def occurrencesAnagrams(occurrences: Occurrences): List[Sentence] = {
    if (occurrences.isEmpty) List(Nil)
    // break this down into wordOccurrences
    //    val occs = sentenceOccurrences(sentence) //letter occurrences of the sentence
    else for {
      // break into combinations of occurrences
      combos <- combinations(occurrences) // all sub-combinations of occurrences

      // find words/sentences from the combination of occurrences
      // check if any given combination exists in the dictionary, return Nil if not
      words <- dictionaryByOccurrences getOrElse(combos, Nil) // all words associated with a given combo of occurrences

      // would need to subtract any used occurrences from the main list of occurrences
      wordsRemain <- occurrencesAnagrams(subtract(occurrences, wordOccurrences(words)))
      // check if all occurrences/letters are used, if yes then it's an anagram, if no, then it's not admissible
      if combos.nonEmpty
      // return the anagram as a Sentence: List[Word]
      // return all anagrams as a List[Sentence]
    } yield words :: wordsRemain
  }
  occurrencesAnagrams(sentenceOccurrences(sentence))
}


// write out what to do given one set of occurrences i.e. convert an occurrence back into it's original word structure


val sentenceTest: Sentence = List("I", "love", "you")
val sentenceTest2: Sentence = List("ate", "likes")
sentenceTest(0)

//dictionaryByOccurrences(wordOccurrences(sentenceTest(1) ++ sentenceTest(0)))

//def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
//
//  def firstAnagrams(sentence: Sentence): Sentence = {
//
//      sentence match {
//        case Nil => List()
//        case xs :: Nil => (wordAnagrams(xs)) //dictionaryByOccurrences(wordOccurrences(sentence(0)))
//        case xs :: ys => wordAnagrams(xs) ++ firstAnagrams(ys)
//      }
//
//  }
//  List(firstAnagrams(sentence))
//}

sentenceAnagrams(sentenceTest2)


wordAnagrams("ate") :: wordAnagrams("love")
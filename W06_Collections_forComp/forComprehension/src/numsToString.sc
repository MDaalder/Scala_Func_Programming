
import scala.io.Source



// translate(phoneNumber)

// example the phone number "7225247386" should have the mnemonic Scala is fun as one element of the set of solution phrases

val in = Source.fromURL("https://www.epfl.ch/labs/lamp/wp-content/uploads/2019/01/linuxwords.txt")

val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))

val mnem = Map(
  '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
  '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

/** Invert the mem map to give a map from chars 'A' ... 'Z' to '2' ... '9' */

val charCode: Map[Char, Char] =
  for ((digit, str) <- mnem; ltr <- str) yield ltr -> digit

/** Maps a word to the digit string it can represent, e.g. "Java" -> "5282" */
def wordCode(word: String): String =
  word.toUpperCase map charCode

wordCode("JAVA") // 5282
wordCode("jaVa") // 5282

/**
 * A map from digit strings to the words that represent them,
* e.g. "5282" -> List("java", "kata", "lava", ...)
* Note: A missing number should map to the empty set, e.g. "1111" -> List()
*/
val wordsForNum: Map[String, Seq[String]] =
  words groupBy wordCode withDefaultValue Seq() // no word associated with that digit string

/** Return all ways to encode a number as a list of words */
def encode(number: String): Set[List[String]] =
  if (number.isEmpty) Set(List())
  else {
    for {
      split <- 1 to number.length // to determine length of first word
      word <- wordsForNum(number take split)
      rest <- encode(number drop split) // remaining number
    } yield word :: rest
  }.toSet

encode("7225247386")

def translate(number: String): Set[String] =
  encode(number) map (_ mkString " ")

translate("7225247386")
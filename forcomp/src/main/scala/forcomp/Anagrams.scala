package forcomp


object Anagrams {

  type Word = String
  type Sentence = List[Word]
  type Occurrence = (Char, Int)
  type Freq = Occurrence
  type Occurrences = List[Occurrence]
  type Freqs = Occurrences
  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = {
    stringToChars(w.toLowerCase).
      groupBy(c => c).
      toList.map{ case (char, list) => new Occurrence(char, list.length) }.
      sortBy{ case (char, _) => char }
  }

  def stringToChars(string: String): List[Char] = string.toList

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(word => wordOccurrences(word)).withDefaultValue(List())

  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  def combs1(list: List[Int]): List[List[Int]] = list.toSet[Int].subsets.map(_.toList).toList
  def combs(list: List[Int]): List[List[Int]] = list match {
    case List() => List(Nil)
    case head :: tail => {
      val tails: List[List[Int]] = combs(tail)
      tails ++ tails.map(l => head::l)
    }
  }

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(Nil)
    case head :: tail => {
      val tails = combinations(tail)
      (for (h <- suboccurrences(head)) yield tails ++ tails.map(l => h :: l)).flatten
    }
  }

  def suboccurrences(occurrence: Occurrence): Occurrences = occurrence match {
    case (char, count) => (for {
      n <- 1 to count
    } yield (char, n)).toList
  } 

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def subtractSingle(x: Occurrences, occ: Occurrence): Occurrences = (x, occ) match {
      case (List(), _) => x
      case (head :: tail, occ) if(head == occ) => tail
      case ((char, count) :: tail, (occ_char, occ_count)) if(char == occ_char && count != occ_count) => (char, count - occ_count) :: tail
      case (head :: tail, occ) => head :: subtractSingle(tail, occ)
    }
    y match {
      case List() => x
      case head :: tail => subtract(subtractSingle(x, head), tail)
    }
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def recur(freqs: Freqs): List[Sentence] = {
      if (freqs.isEmpty) List(List())
      else {
        for {
          combination <- combinations(freqs)
          word <- dictionaryByOccurrences(combination)
          rest <- recur(subtract(freqs, combination))
        } yield word :: rest
      }
    }

    recur(sentenceOccurrences(sentence))
  }
}

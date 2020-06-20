package forcomp

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   * how often the character appears.
   * This list is sorted alphabetically w.r.t. to the character in each pair.
   * All characters in the occurrence list are lowercase.
   *
   * Any list of pairs of lowercase characters and their frequency which is not sorted
   * is **not** an occurrence list.
   *
   * Note: If the frequency of some character is zero, then that character should not be
   * in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   * It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   * Note: the uppercase and lowercase version of the character are treated as the
   * same character, and are represented as a lowercase character in the occurrence list.
   *
   * Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = {
    val charmap = (w.toLowerCase.toList groupBy (x => x))
    (for ((key, value) <- charmap.toList) yield (key, value.length)).sorted
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString(""))
  }

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   * the words that have that occurrence count.
   * This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   * For example, the word "eat" has the following character occurrence list:
   *
   * `List(('a', 1), ('e', 1), ('t', 1))`
   *
   * Incidentally, so do the words "ate" and "tea".
   *
   * This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   * List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    val wordoccurpair = (for (word <- dictionary) yield (wordOccurrences(word), word))
    wordoccurpair.groupMap(x => x._1)(x => x._2)
  } withDefaultValue List()

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    dictionaryByOccurrences(wordOccurrences(word))
  }

  /** Returns the list of all subsets of the occurrence list.
   * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   * is a subset of `List(('k', 1), ('o', 1))`.
   * It also include the empty subset `List()`.
   *
   * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   * List(
   * List(),
   * List(('a', 1)),
   * List(('a', 2)),
   * List(('b', 1)),
   * List(('a', 1), ('b', 1)),
   * List(('a', 2), ('b', 1)),
   * List(('b', 2)),
   * List(('a', 1), ('b', 2)),
   * List(('a', 2), ('b', 2))
   * )
   *
   * Note that the order of the occurrence list subsets does not matter -- the subsets
   * in the example above could have been displayed in some other order.
   */

  def subset_generator(value: List[Char]): List[List[Char]] = {
    if (value.isEmpty) List(List())
    else {
      val x = for (elt <- subset_generator(value.tail)) yield (elt)
      val y = for (elt <- subset_generator(value.tail)) yield (value.head :: elt)
      x ::: y
    }
  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    val chars: List[Char] = (for (elt <- occurrences; index <- 1 to elt._2) yield elt._1)
    val all_subsets = subset_generator(chars)
    for (elt <- all_subsets) yield wordOccurrences(elt.mkString(""))
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   * The precondition is that the occurrence list `y` is a subset of
   * the occurrence list `x` -- any character appearing in `y` must
   * appear in `x`, and its frequency in `y` must be smaller or equal
   * than its frequency in `x`.
   *
   * Note: the resulting value is an occurrence - meaning it is sorted
   * and has no zero-entries.
   */

  def findelt(tuple: (Char, Int), y: Occurrences): List[(Char, Int)] = {
    if (y.isEmpty) List(tuple)
    else if (tuple._1 == y.head._1 && tuple._2 == y.head._2) List()
    else if (tuple._1 == y.head._1 && tuple._2 > y.head._2) List((tuple._1, tuple._2 - y.head._2))
    else findelt(tuple, y.tail)
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    if (x.isEmpty) List() else {
      findelt(x.head, y) ::: subtract(x.tail, y)
    }
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   * An anagram of a sentence is formed by taking the occurrences of all the characters of
   * all the words in the sentence, and producing all possible combinations of words with those characters,
   * such that the words have to be from the dictionary.
   *
   * The number of words in the sentence and its anagrams does not have to correspond.
   * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   * Also, two sentences with the same words but in a different order are considered two different anagrams.
   * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   * `List("I", "love", "you")`.
   *
   * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   * List(
   * List(en, as, my),
   * List(en, my, as),
   * List(man, yes),
   * List(men, say),
   * List(as, en, my),
   * List(as, my, en),
   * List(sane, my),
   * List(Sean, my),
   * List(my, en, as),
   * List(my, as, en),
   * List(my, sane),
   * List(my, Sean),
   * List(say, men),
   * List(yes, man)
   * )
   *
   * The different sentences do not have to be output in the order shown above - any order is fine as long as
   * all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   * so it has to be returned in this list.
   *
   * Note: There is only one anagram of an empty sentence.
   */
//Source for this function: StackOverflow
  def partitions[T](list: List[T]): List[List[List[T]]] =
    list match {
      case Nil | _ :: Nil => // 0/1 elements
        List(List(list))
      case head :: tail => // 2+ elements
        partitions(tail).flatMap(part => {
          val joins =
            part.indices.map(i =>
              part.zipWithIndex.map { case (p, j) =>
                if (i == j) {
                  head +: p
                } else {
                  p
                }
              }
            )

          (List(head) +: part) +: joins
        })
    }

  def isvalid(value: List[List[Char]]): Boolean = {
    if (value.isEmpty) true else if (wordAnagrams(value.head.mkString("").toLowerCase).isEmpty) false else isvalid(value.tail)
  }

  def getcartprod(value: List[List[Anagrams.Word]]): List[Sentence] = {
    if (value.tail.isEmpty) {
      val list = value.head
      for (elt <- list) yield List(elt)
    } else
      {
        val list1 = value.head
        val list2 = getcartprod(value.tail)
        for (elt1 <- list1; elt2 <- list2) yield elt1 :: elt2
      }
  }

  def gensentence(value: List[List[Char]]): List[Sentence] = {
    val alllists = for(elt <- value) yield wordAnagrams(elt.mkString("").toLowerCase)
    getcartprod(alllists)
  }

  def sentence_helper(partition: List[List[List[Char]]]): List[Sentence] =
  {
    if (partition.isEmpty) List() else gensentence(partition.head) ::: sentence_helper(partition.tail)
  }

  def getperm(value: List[Anagrams.Sentence]): List[Sentence] = {
    if (value.isEmpty) List() else value.head.permutations.toList ::: getperm(value.tail)
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) List(List()) else {
      val partition = (partitions(sentence.mkString("").toList))
      val validones = for (elt <- partition if isvalid(elt)) yield (elt)
      val x = sentence_helper(validones).toSet.toList
      getperm(x)
    }
  }
}
object Dictionary {
  def loadDictionary: List[String] = {
    val wordstream = Option {
      getClass.getResourceAsStream(List("forcomp", "linuxwords.txt").mkString("/", "/", ""))
    } getOrElse {
      sys.error("Could not load word list, dictionary file not found")
    }
    try {
      val s = scala.io.Source.fromInputStream(wordstream)
      s.getLines.toList
    } catch {
      case e: Exception =>
        println("Could not load word list: " + e)
        throw e
    } finally {
      wordstream.close()
    }
  }
}


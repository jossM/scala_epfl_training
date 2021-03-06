package forcomp

object Anagrams extends AnagramsInterface {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = Dictionary.loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase.foldLeft(Map.empty[Char, Int])(
    (charCount, char) => charCount + (char -> (charCount.getOrElse(char, 0) + 1))
  ).toList.sortBy(_._1)

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)

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
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word), List())

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
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    @scala.annotation.tailrec
    def combinationsRec(occurrencesLeft: Occurrences, previousCombinations: List[Occurrences]): List[Occurrences] = {
      if (occurrencesLeft.isEmpty) previousCombinations
      else {
        val (char, maxCount) = occurrencesLeft.head
        val newPreviousCombinations: List[Occurrences] = {
          for {
            count <- 0 to maxCount
            occurrence <- previousCombinations // we study previousCombinations.length * count element
          } yield if (count <= 0) occurrence else (char, count) :: occurrence
        }.toList
        combinationsRec(occurrencesLeft.tail, newPreviousCombinations)
      }
    }
    combinationsRec(occurrences.reverse, List(List())) // we need at least one element in previousCombinations for "for expr."
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
  def convertToMap(occurrences: Occurrences): Map[Char, Int] = {
    occurrences
      .groupBy(_._1)
      .view.mapValues(elements => elements.foldLeft(0)((count, pair) => count + pair._2)).toMap
  }
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = convertToMap(x)
    val yMap = convertToMap(y)
    assert(yMap forall { case (char, yCount) => xMap.getOrElse(char, 0) >= yCount})

    x flatMap { case (char, xCount) =>
      val yCount = yMap.getOrElse(char, 0)
      if (xCount <= yCount) List() else List((char, xCount - yCount))
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
    def computeValidOccurrences(occurrencesLeft: Occurrences,
                                selectedOccurrences: List[Occurrences]): Set[List[Occurrences]] = {
      if (occurrencesLeft.isEmpty)
        Set(selectedOccurrences)
      else {
        val allWordsOccurrences = combinations(occurrencesLeft).filter(dictionaryByOccurrences.contains)
        if (allWordsOccurrences.isEmpty)
          Set()
        else {
          allWordsOccurrences flatMap { singleOccurrences: Occurrences =>
            computeValidOccurrences(
              subtract(occurrencesLeft, singleOccurrences),
              singleOccurrences :: selectedOccurrences)
          }
        }.toSet
      }
    }

    val sentenceCharOccurrences = sentenceOccurrences(sentence)
    val allOccurrencesSequences = computeValidOccurrences(sentenceCharOccurrences, List()).toList

    @scala.annotation.tailrec
    def toSentences(reversedOccurrencesSeq: List[Occurrences], partSentences: List[Sentence]): List[Sentence] = {
      if (reversedOccurrencesSeq.isEmpty)
        partSentences
      else {
        val correspondingWords = dictionaryByOccurrences.getOrElse(reversedOccurrencesSeq.head, List())
        val newSentences: List[Sentence] = correspondingWords flatMap { word => partSentences.map(word :: _)}
        toSentences(reversedOccurrencesSeq.tail, newSentences)
      }
    }

    allOccurrencesSequences.flatMap(occurrencesSeq => toSentences(occurrencesSeq.reverse, List(List())))
  }

  // Bonus : uses dynamic programming instead of suggested solution
  // This builds a a dynamic mapping to the solution avoiding recomputing solutions that have already been studied
  // The solution adapts from problems issued from string edit distance algorithm and should be vastly superior
  // to the previous one
  type CharOccurrences = Map[Char, Int]

  def toOccurrences(charOccurrences: CharOccurrences): Occurrences = charOccurrences.toList

  lazy val dictionaryWithoutEmptyString: Map[CharOccurrences, Set[String]] = {
    dictionary
      .groupBy(word => convertToMap(wordOccurrences(word)))
      .view.mapValues(_.toSet).toMap
  } - Map.empty[Char, Int] // we remove the possibility of having an empty string

  def isSubOccurrences(occurrences: CharOccurrences)(subOccurrences: CharOccurrences): Boolean =
    subOccurrences forall { case (char, count) => occurrences.getOrElse(char, 0) >= count }

  def addCharOccurrences(largerOccurrences: CharOccurrences, smallerOccurrences: CharOccurrences): CharOccurrences =
    smallerOccurrences.foldLeft(largerOccurrences)(
      (aggregate, charCount) => charCount match {
      case (char, count) => aggregate + { char -> {count + aggregate.getOrElse(char, 0)} }
    })

  def subCharOccurrences(occurrences: CharOccurrences, occurrencesToSubtract: CharOccurrences): CharOccurrences = {
    assert(isSubOccurrences(occurrences)(occurrencesToSubtract))
    occurrencesToSubtract.foldLeft(occurrences)(
      (aggregate, charCountPair) => {
        val (char, charCount) = charCountPair
        val aggregateCharCount = aggregate.getOrElse(char, 0)
        if (charCount == aggregateCharCount)
          aggregate - char
        else // charCount < aggregateCharCount
          aggregate + {
            char -> {
              aggregateCharCount - charCount
            }
          }
      })
  }

  type ExplorationMap = Map[CharOccurrences, List[CharOccurrences]]

  def findAnagrams(sentence: Sentence): List[Sentence] = { // findAnagrams
    val targetCharOccurrences = convertToMap(sentenceOccurrences(sentence))
    def isTargetSubOccurrences = isSubOccurrences( targetCharOccurrences )(_)

    @scala.annotation.tailrec
    def findAnagramsRec(occurrencesPath: ExplorationMap,
                        dictionaryLeft: Map[CharOccurrences, Set[Word]],
                        previouslyExploredOccurrences: Set[CharOccurrences]): ExplorationMap = {
      if (previouslyExploredOccurrences.isEmpty) // if we have no more path to explore we give up
        occurrencesPath // We have all the solutions
      else {
        val deltaOccurrences: List[(CharOccurrences, CharOccurrences, CharOccurrences)] =
          previouslyExploredOccurrences.toList flatMap {
            occurrences => {
              for {
                (wordOccurrences: CharOccurrences, _) <- dictionaryLeft
                sumOccurrences = addCharOccurrences(occurrences, wordOccurrences)
                if (isTargetSubOccurrences(sumOccurrences))
              } yield (sumOccurrences, occurrences, wordOccurrences)
            }
          }
        // find what we haven't explored yet
        val newlyFoundOccurrences = deltaOccurrences.map(_._1).toSet.filterNot(occurrencesPath.contains)
        // filter dictionary
        val usedWordsCharCount = deltaOccurrences.map(_._3).toSet
        val filteredDictionary = dictionaryLeft filter { case (charCount, _) => usedWordsCharCount.contains(charCount) }
        //  add new path that we just found
        val newOccurrencesPath = deltaOccurrences.foldLeft(occurrencesPath)((aggregate, delta) => {
          val (nextOccurrences, previousOccurrences, _) = delta
          val previousPredecessors = aggregate.getOrElse(nextOccurrences, List())
          if (previousPredecessors.contains(previousOccurrences))
            aggregate
          else
            aggregate + { nextOccurrences -> { previousOccurrences :: previousPredecessors } }
        })
        findAnagramsRec(newOccurrencesPath, filteredDictionary, newlyFoundOccurrences)
      }
    }

    val initialCombination: CharOccurrences = Map.empty[Char, Int]
    val occurrencesPath = findAnagramsRec(
      Map(initialCombination -> List()),
      dictionaryWithoutEmptyString filter { case (charCount, _) => isTargetSubOccurrences(charCount) },
        Set(initialCombination)
    )

    def getOccurrencesPaths(occurrencesPointer: CharOccurrences): List[List[CharOccurrences]] = {
      if (occurrencesPointer.isEmpty)
        List(List())
      else
        occurrencesPath.get(occurrencesPointer) match {
          case Some(allAncestors) =>
            allAncestors flatMap {ancestor =>
              val word = subCharOccurrences(occurrencesPointer, ancestor)
              getOccurrencesPaths(ancestor).map(word :: _)
            }
        }
    }

    val paths = getOccurrencesPaths(targetCharOccurrences)

    @scala.annotation.tailrec
    def toSentences(reversedOccurrencesSeq: List[CharOccurrences], partSentences: List[Sentence]): List[Sentence] = {
      if (reversedOccurrencesSeq.isEmpty)
        partSentences
      else {
        val correspondingWords = dictionaryWithoutEmptyString.getOrElse(reversedOccurrencesSeq.head, List())
        val newSentences = correspondingWords.flatMap(word => partSentences.map(word :: _)).toList
        toSentences(reversedOccurrencesSeq.tail, newSentences)
      }
    }

    paths .flatMap(charOccurrencesPath => toSentences(charOccurrencesPath, List(List())))
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

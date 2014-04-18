package textcompose.core

import scala.collection.mutable.HashMap

class WordVectors {

  private val wordIndex = new HashMap[String, Long]
  private val indexWord = new HashMap[Long, String]
  private var nextWordIndex = 0L

  class Vector(position: Long) {
    val wordCount = new HashMap[Long, Int]

    def add(word: String) {
      if (!wordIndex.contains(word)) {
        wordIndex(word) = nextWordIndex
        indexWord(nextWordIndex) = word
        nextWordIndex += 1
      }
      val index = wordIndex(word)
      if (wordCount.contains(index)) {
        wordCount(index) += 1
      } else {
        wordCount(index) = 1
      }
    }
  }

  private val grossVector = new Vector(0)
  private val vectors = new HashMap[Int, Vector]
  private var inParagraph = false
  private var currentVector: Vector = null

  def addToParagraph(text: String, position: Int) {
    if (!inParagraph) {
      currentVector = new Vector(position)
      vectors(position) = currentVector
    }
    val words = text.split(' ')
    for (w <- words) {
      currentVector.add(w)
      grossVector.add(w)
    }
  }

  def endParagraph() {
    inParagraph = false
  }

  def report() {
    println("----")
    for ((i, c) <- grossVector.wordCount) {
      if (c > 2) println(indexWord(i) + "\t" + c.toString)
    }
  }
}
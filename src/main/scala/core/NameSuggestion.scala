/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.ArrayBuffer

object NameSuggestion {

  /** Levenshtein distance between words */
  private def LevenshteinDistance(a: String, b: String): Int = {

    def min(x1: Int, x2: Int, x3: Int): Int = {
      if (x1 < x2) { if (x1 < x3) x1 else x3 } else { x2 }
    }

    val aTop = a.length - 1
    val bTop = b.length - 1
    var d = Array.ofDim[Int](aTop + 1, bTop + 1)

    for (i <- 0 to aTop) d(i)(0) = i // deletion
    for (j <- 0 to bTop) d(0)(j) = j // deletion
    for (j <- 1 to bTop) {
      for (i <- 1 to aTop) {
        if (a(i) == b(j)) {
          d(i)(j) = d(i - 1)(j - 1)
        } else {
          d(i)(j) = min(d(i - 1)(j) + 1, // deletion
            d(i)(j - 1) + 1, // insertion
            d(i - 1)(j - 1) + 1) // substitution
        }
      }
    }
    d(aTop)(bTop)
  }

  
  /** Get list of strings from list of string which are close to some string
    * in the Levenshtein metric. 
    */
  
  def getSuggestions(a: String, strings: List[String]): String = {
    var Candidates = new ArrayBuffer[String]
    var minDistance = 0
    for (t <- strings) {
      val d = LevenshteinDistance(a, t)
      if (d < minDistance || minDistance == 0) {
        if (minDistance > 0) Candidates.clear()
        minDistance = d
      }
      if (d == minDistance) {
        Candidates += t
      }
    }
    val count = Candidates.size

    if (count > 0 && minDistance < a.length / 2 + 1) {
      var result = ""
      var index = 1
      for (c <- Candidates) {
        if (index == 1) {
          result = c
        } else if (index < count) {
          result += ", " + c
        } else {
          result += " or " + c
        }
        index += 1
      }
      return " Did you mean " + result + "?"
    } else {
      return ""
    }
  }

}
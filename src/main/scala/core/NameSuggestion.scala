/*
 * Writesetter is a program for creating PDF documents from text files with markup.
 * Copyright (c) 2013 Jesper S Villadsen <jeschvi@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package writesetter.core

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
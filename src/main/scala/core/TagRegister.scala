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

import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

object TagRegister {

  private var TagNames = new HashSet[String]

  def addBuiltInTags {
    TagNames += "extension"
    TagNames += "def"
    TagNames += "/def"
    TagNames += "sub"
    TagNames += "/sub"
    TagNames += "main"
    TagNames += "/main"
    TagNames += "template"
    TagNames += "font"
    TagNames += "size"
    TagNames += "face"
    TagNames += "color"
    TagNames += "underline"
    TagNames += "highlight"
    TagNames += "/highlight"
    TagNames += "letter-spacing"
    TagNames += "scale-letter"
    TagNames += "image"
    TagNames += "scale-image"
    TagNames += "fit-image"
    TagNames += "rotate-image"
    TagNames += "frame"
    TagNames += "blend"
    TagNames += "opacity"
    TagNames += "bookmark"
    TagNames += "label"
    TagNames += "ref"
    TagNames += "/ref"
    TagNames += "rise"
    TagNames += "align"
    TagNames += "indent"
    TagNames += "height"
    TagNames += "document"
    TagNames += "viewer"
    TagNames += "margins"
    TagNames += "page-size"
    TagNames += "orientation"
    TagNames += "columns"
    TagNames += "new"
    TagNames += "paragraph-space"
    TagNames += "paragraph-indent"
    TagNames += "char"
    TagNames += "Roman"
    TagNames += "format-list"
    TagNames += "list"
    TagNames += "item"
    TagNames += "/list"
    TagNames += "table"
    TagNames += "/table"
    TagNames += "cell"
    TagNames += "cell-padding"
    TagNames += "border-width"
    TagNames += "border-color"
    TagNames += "move-to"
    TagNames += "line-width"
    TagNames += "line-cap"
    TagNames += "line-dash"
    TagNames += "line-to"
    TagNames += "draw"
    TagNames += "position"
    TagNames += "inject"
    TagNames += "help"
    TagNames += "loop"
    TagNames += "whitespace"
    TagNames += "store"
    TagNames += "restore"
    TagNames += "reset"
    TagNames += "var"
    TagNames += "set"
    TagNames += "add"
    TagNames += "show"
    TagNames += "replace"
    TagNames += "insert"
    TagNames += "include"
    TagNames += "view"
    TagNames += "encrypt"
    TagNames += "/set"
    TagNames += "/add"
  }

  def AddNewTag(t: String) { TagNames += t }

  /*
	 * This general purpose distance function should not hide here!
	 */
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

  /*
	 * This function could be generalized to compare a string to a given list - not just the list of tags.
	 * It could also be used for the list of fonts.
	 */
  def GetSuggestions(a: String): String = {
    var Candidates = new ArrayBuffer[String]
    var minDistance = 0
    for (t <- TagNames) {
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
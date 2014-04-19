/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import java.util.regex.Pattern
import java.util.regex.Matcher

class ReplacementRegister {

  class Replacement(
    kind: String,
    ord: Int,
    findRegEx: String,
    replaceWith: String,
    ignoreCase: Boolean,
    applyToTags: Boolean) {

    var flags = 0
    if (ignoreCase) { flags = Pattern.CASE_INSENSITIVE }

    private val findPattern = Pattern.compile(findRegEx, flags)

    private var order = ord

    private val nameOfReplacementTag = "replacement of '" + findRegEx + "'"

    private def findAndReplaceAllMatches(m: Matcher): String = {
      val sb = new StringBuffer
      while (m.find) {
        m.appendReplacement(sb, replaceWith)
      }
      m.appendTail(sb)
      sb.toString
    }

    def getKind: String = kind

    def setOrder(ord: Int) { order = ord }

    def getOrder: Int = order

    def doApply(policy: String): Boolean = policy == "yes" || policy == "tag" && applyToTags

    def apply(s: String): String = {
      try {
        val findMatcher = findPattern.matcher(s) // create the pattern matcher
        findAndReplaceAllMatches(findMatcher)
      } catch {
        case e: Exception => throw new TagError("Replacement failed: " + e.getMessage + ".")
      }
    }
  }

  private var replacements = new HashMap[String, Replacement] // id -> replacement

  private var ordering = new HashMap[String, ArrayBuffer[String]] // kind -> (id -> replacement)

  def updateOrderedList(kind: String) {
    val temp = new HashMap[Int, ArrayBuffer[String]] // order -> list of id's with that order
    var maxOrder = 0
    for (id <- ordering(kind)) {
      val order = replacements(id).getOrder
      if (order > 0) {
        if (!temp.isDefinedAt(order)) temp(order) = new ArrayBuffer[String]
        temp(order) += id
        if (order > maxOrder) maxOrder = order
      }
    }
    ordering(kind).clear()
    for (i <- 1 to maxOrder) {
      if (temp.isDefinedAt(i)) {
        for (id <- temp(i)) {
          ordering(kind) += id
        }
      }
    }
  }

  def add(
    kind: String,
    order: Int,
    id: String,
    findRegEx: String,
    replaceWith: String,
    ignoreCase: Boolean,
    applyToTags: Boolean) {

    if (replacements.contains(id)) {
      throw new TagError("A replacement with id '" + id + "' has already been declared.")
    }
    try {
      var re = new Replacement(kind, order, findRegEx, replaceWith, ignoreCase, applyToTags)
      replacements += id -> re
    } catch {
      case e: Exception => throw new TagError(e.getMessage + " in pattern for 'replace' tag '" + findRegEx + "'.")
    }
    if (!ordering.isDefinedAt(kind)) ordering(kind) = new ArrayBuffer[String]
    ordering(kind) += id
    updateOrderedList(kind)
  }

  def setOrder(id: String, order: Int) {
    if (order != replacements(id).getOrder) {
      replacements(id).setOrder(order)
      updateOrderedList(replacements(id).getKind)
    }
  }

  def apply(kind: String, s: String, replacementPolicy: String): String = {
    if (replacementPolicy == "no") {
      s
    } else {
      var result = s
      if (ordering.isDefinedAt(kind)) {
        for (id <- ordering(kind)) {
          if (replacements(id).doApply(replacementPolicy)) {
            result = replacements(id).apply(result)
          }
        }
      }
      result
    }
  }
}
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

import scala.collection.mutable.Stack

class InjectionRegister {

  class Injection(point: String, ba: String, oe: String, ob: String, n: Int, c: String, d: String, r: Int) {
    var pointDescription = point
    var pointBeforeAfter = ba
    var pointOddEven = oe
    var pointObject = ob
    var pointNumber = n
    var content = c
    var description = d
    var priority = r

    def longDescription: String = {
      var r = "injection "
      if (description != "") r += "'" + description + "' "
      r += "at '" + pointDescription + "'"
      return r
    }

    def ToString: String = {
      pointDescription + ": " + pointBeforeAfter + ", " + pointOddEven + ", " + pointObject + ", " + pointNumber.toString + ", C=" + content + " D=" + description + " P=" + priority.toString
    }
  }

  private var injections = new Stack[Injection]

  def getContent(index: Int): String = injections(index).content

  def addInjection(point: String, content: String, d: String, r: Int) {

    // SYNTAX for injection point: before/after [odd/even] row/column/page-break/all [1, 2, 3,...]

    var pl = point.split(' ')
    val l = pl.length
    var ba = ""
    var oe = ""
    var ob = ""
    var n = 0

    if (l > 0 && (pl(0) == "before" || pl(0) == "after")) {
      ba = pl(0)
    } else {
      throw new TagError("The injection point must start with 'before' or 'after'.")
    }
    var index = 1
    if (l > 1 && (pl(1) == "odd" || pl(1) == "even")) {
      oe = pl(1)
      index = 2
    }
    if (l <= index) throw new TagError("Please specify an injection point: 'row', 'column', 'page-break' or 'all'.")
    if (pl(index) == "row" || pl(index) == "column" || pl(index) == "page-break" || pl(index) == "all") {
      ob = pl(index)
      index += 1
    } else {
      throw new TagError("Unknown injection point '" + pl(index) + "'. Try 'row', 'column', 'page-break' or 'all', optionally preceeded by odd/even or followed by a number.")
    }
    if (ba == "after" && ob == "page-break") {
      throw new TagError("Injection after page-break is not permitted. Try 'before page-break' instead.")
    }
    if (l > index) {
      try {
        n = pl(index).toInt
        index += 1
      } catch {
        case e: Exception => throw new TagError("The last part of the injection point (" + pl(index) + ") is not a number.")
      }
    }
    if (l > index) throw new TagError("The injection point is too long: '" + pl(index) + "'?")
    if (n < 0) throw new TagError("The injection point cannot contain a negative number.")
    if (oe != "" && n > 0) throw new TagError("The injection point cannot both contain odd/even and a number.")
    if (content == "") throw new TagError("The injection content is empty.")
    injections.push(new Injection(point, ba, oe, ob, n, content, d, r))
  }

  def getApplicableInjections(ba: String, ob: String, n: Int): String = {
    var r = ""
    var oe = ""
    if (n % 2 == 0) {
      oe = "even"
    } else {
      oe = "odd"
    }

    var index = 0
    val length = injections.length
    while (index < length) {
      val inj = injections(index)
      if (inj.pointBeforeAfter == ba && inj.pointObject == ob
        && (inj.pointOddEven == oe || inj.pointNumber == n || (inj.pointOddEven == "" && inj.pointNumber == 0))) {
        r += index.toString + " "
      }
      index += 1
    }
    return r
  }
}

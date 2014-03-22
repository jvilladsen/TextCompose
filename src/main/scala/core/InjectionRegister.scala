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

  class Injection(ba: String, oe: String, point: String, n: Int, c: String, d: String, r: Int) {
    var pointBeforeAfter = ba
    var pointOddEven = oe
    var pointObject = point
    var pointNumber = n
    var content = c
    var description = d
    var priority = r
  }

  private var injections = new Stack[Injection]

  def getContent(index: Int): String = injections(index).content

  def addInjection(beforeOrAfter: String, oddOrEven: String, point: String, number: Int, content: String, d: String, r: Int) {

    if (beforeOrAfter == "after" && point == "page-break") {
      throw new TagError("Injection after page-break is not permitted. Try 'before page-break' instead.")
    }
    if (number < 0) throw new TagError("The injection point cannot contain a negative number.")
    if (oddOrEven != "" && number > 0) throw new TagError("The injection point cannot both contain odd/even and a number.")
    if (content == "") throw new TagError("The injection content is empty.")
    injections.push(new Injection(beforeOrAfter, oddOrEven, point, number, content, d, r))
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

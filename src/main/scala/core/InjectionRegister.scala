/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

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

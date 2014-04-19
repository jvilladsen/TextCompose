/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

object NumberFunctions {

  val values = List(1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1)
  val numerals = List("M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I")

  private def arabicToRoman(number: Int): String = {
    var result = ""
    var remainder = number
    var index = 0
    for (v <- values) {
      while (remainder >= v) {
        remainder -= v
        result += numerals(index)
      }
      index += 1
    }
    result
  }

  def getRomanNumber(lowerCase: Boolean, number: Int): String = {
    var result = ""
    if (number < 0 || number > 3999) {
      throw new TagError("Only supports presenting numbers between 0 and 3999 as Roman numbers")
    } else if (number == 0) {
      result = "N"
    } else {
      result = arabicToRoman(number)
    }
    if (lowerCase) result.toLowerCase else result
  }

  def getNumber(s: String): Int = {
    if (s(0) == '#') {
      try {
        Integer.valueOf(s.substring(1), 16).intValue // HEX -> INT
      } catch {
        case e: Exception => throw new TagError("'" + s.substring(1) + "' is not a hexadecimal number.")
      }
    } else {
      try {
        s.toInt
      } catch {
        case e: Exception => throw new TagError("'" + s + "' is not a integer (in decimal notation).")
      }
    }
  }

  def getFloat(s: String, purpose: String): Float = {
    try {
      s.toFloat
    } catch {
      case e: Exception => throw new TagError(purpose + " should be a number. You wrote '" + s + "'.")
    }
  }
}
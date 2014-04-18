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
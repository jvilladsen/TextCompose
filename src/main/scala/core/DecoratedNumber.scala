/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

class DecoratedNumber(purpose: String) extends Cloneable {

  def this(element: String, purpose: String) = {
    this(purpose)
    this.parse(element)
  }

  var decoration = ""
  var isDelta = false // Is set to true if the number starts with + or - and we should interpret this as a delta.
  var value = 0f
  var hasSign = false
  private var interpretSignAsDelta = false

  override def clone: this.type = super.clone.asInstanceOf[this.type]

  def doInterpretSignAsDelta() { interpretSignAsDelta = true }

  def parse(element: String) {
    var firstCharacter = true
    var negative = false
    var numberFound = false
    var numberPart = ""
    decoration = ""
    isDelta = false
    var alfaPartStarted = false
    for (C <- element) {
      if (firstCharacter && C == '+') {
        isDelta = interpretSignAsDelta
        hasSign = true
      } else if (firstCharacter && C == '-') {
        isDelta = interpretSignAsDelta
        hasSign = true
        negative = true
      } else if (!alfaPartStarted && (C >= '0' && C <= '9' || C == '.')) {
        numberFound = true
        numberPart += C
      } else {
        alfaPartStarted = true
        decoration += C
      }
      firstCharacter = false
    }
    if (numberFound) {
      try {
        value = numberPart.toFloat
      } catch {
        case e: Exception => throw new DecorationError(errorPrefix + "should be a number. You wrote " + numberPart + ".")
      }
      if (negative) {
        value = -value
      }
    } else {
      throw new DecorationError(errorPrefix + "should contain a number.")
    }
  }

  // Works on two decorated numbers with either no decoration or decoration = "%"
  // Modifies self with the provided delta
  def applyChange(change: DecoratedNumber) {
    if (change.decoration != "" && change.decoration != "%") {
      throw new DecorationError(errorPrefix + "should be a number. Prefix +/- or none, Postfix % or none, e.g. 50, +50%.")
    }
    if (change.isDelta) {
      if (change.decoration == "%") {
        if (change.value > 0) {
          value = (value * (100f + change.value)) / 100f
        } else {
          value = (value * 100f) / (100f - change.value)
        }
      } else {
        value = value + change.value
      }
    } else {
      decoration = change.decoration
      value = change.value
    }
  }

  // Applies the decorated number to a number, possibly isDelta, and returns the result.
  // The decorated number must have either no decoration or decoration = "%".
  def applyToNumber(n: Float): Float = {
    if (decoration == "%") {
      if (isDelta) {
        if (value > 0) {
          (n * (100f + value)) / 100f
        } else {
          (n * 100f) / (100f - value) // the value is negative, so change of sign
        }
      } else {
        (n * value) / 100f
      }
    } else if (decoration == "") {
      if (isDelta) {
        n + value
      } else {
        value
      }
    } else {
      throw new DecorationError(errorPrefix + "should be a number. Prefix +/- or none, Postfix % or none, e.g. 50, +50%.")
    }
  }

  def getValueOrPercentageOfNumber(base: Float): Float = {
    // Returns the value if no decoration, and returns the result of applying the value
    // as a percentage of the provided base parameter - if the decoration is "%".
    var result = 0f
    if (decoration == "%") {
      result = (value / 100f) * base
    } else if (decoration == "") {
      result = value
    } else {
      throw new DecorationError(errorPrefix + "should be a number. Postfix % or none, e.g. 50, 50%.")
    }
    result.toFloat
  }

  def getValueOfPercentagMulti(decor: List[String], base: List[Float]): Float = {
    // Returns the value if no decoration. Otherwise applies the value as percentage to the base.
    if (decoration == "") {
      value
    } else if (decor.contains(decoration)) {
      val index = decor.indexOf(decoration)
      val baseNumber = base(index)
      (value / 100f) * baseNumber
    } else {
      val allowed = decor.map(d => "'" + d + "'").mkString(", ")
      throw new TagError(purpose + " should be a number, optionally post-fixed with one of: " + allowed + ".")
    }
  }

  private def errorPrefix: String =
    if (purpose != "") { "'" + purpose + "' " } else { "" }

  override def toString: String =
    (if (isDelta && value > 0) "+" else "") + value.toString + decoration

  def IsEqualTo(dn: DecoratedNumber): Boolean = {
    decoration == dn.decoration && isDelta == dn.isDelta && value == dn.value
  }
}

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

package writesetter.tagGUI

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import writesetter.{ core, editor }

class NumberType(
  tagName: String,
  label: String,
  allowDelta: Boolean,
  integer: Boolean,
  decor: List[String],
  percentageOption: Boolean,
  forcedPercentage: Boolean) extends ParameterType {

  /*
	 * forcedPercentage : use this for numbers that are always a percentage (so not an option in the dialog)
	 * 					  and decorated with a percentage sign in the source code.
	 * 					  Example: opacity in image tag. 
	 */

  def this(tagName: String, label: String) = {
    this(tagName, label, false, false, List(), false, false)
  }
  def this(tagName: String, label: String, integer: Boolean) = {
    this(tagName, label, false, integer, List(), false, false)
  }
  def this(tagName: String, label: String, decor: List[String]) = {
    this(tagName, label, false, false, decor, false, false)
  }

  private val labelLabel = new Label {
    horizontalAlignment = Alignment.Right
    foreground = editor.Colors.standard
    text = label
  }
  private val deltaField = new ComboBoxType("", List("+", "-"), false)
  private val valueField = new TextField {
    columns = 13
    text = "0"
  }
  private val decoration = new ComboBoxType("", decor, false)
  private val percentageField = new CheckBox {
    horizontalAlignment = Alignment.Right
  }
  private val percentageLabel = new Label {
    text = "percentage"
    foreground = editor.Colors.standard
  }

  private var defaultValue = 0f

  var columns = 1
  if (label != "") { columns += 1 }
  if (allowDelta) { columns += 1 }
  if (!decor.isEmpty) { columns += 1 }
  if (percentageOption) { columns += 2 }

  private val layoutGroup = new GridPanel(1, columns) {
    if (label != "") { contents += labelLabel }
    if (allowDelta) { contents += deltaField.panel }
    contents += valueField
    if (!decor.isEmpty) { contents += decoration.panel }
    if (percentageOption) {
      contents += percentageField
      contents += percentageLabel
    }
  }

  AddToPanel(layoutGroup, true)

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(valueField.keys)
    panel.reactions += {
      case KeyPressed(`valueField`, Enter, _, _) => action.apply()
    }
  }

  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {

    if (parameters.length > offset && parameters(offset) != "") {
      var DN = new core.DecoratedNumber(tagName)
      if (allowDelta) DN.doInterpretSignAsDelta
      try {
        DN.parse(parameters(offset))
      } catch {
        case e: Exception => return 0 // FIXME: pass on the message to the user.
      }

      if (allowDelta && DN.isDelta) {
        val sign = if (DN.value > 0) "+" else if (DN.value < 0) "-" else ""
        deltaField.Set(ArrayBuffer(sign), 0)
      }

      val valueForText = if (allowDelta) DN.value.abs else DN.value
      valueField.text = if (integer) valueForText.toInt.toString else valueForText.toString

      if (decor.contains(DN.decoration)) {
        decoration.Set(ArrayBuffer(DN.decoration), 0)
      }
      percentageField.selected = percentageOption && DN.decoration == "%"
      1
    } else {
      valueField.text = if (integer) defaultValue.toInt.toString else defaultValue.toString
      0
    }
  }

  def grabFocus { valueField.peer.grabFocus }

  def IsValid: Boolean = {
    var result = true
    if (valueField.text == "") {
      result = !mandatory
    } else {
      try {
        val x = valueField.text.toFloat
      } catch {
        case e: Exception => result = false
      }
    }
    return result
  }

  private def IsZeroOrEmpty(text: String): Boolean = {
    if (text == "") {
      true
    } else {
      try {
        val x = text.toFloat
        return x.abs < 0.000001f
      } catch {
        case e: Exception => return true
      }
    }
  }

  def Get: String = {

    def valueMadeEasy = {
      /*
			 * If the type is "integer" this is the value before the decimal point.
			 * Float's tend to get shown with a ".0" at the end. This is fine, since
			 * you get information about the possibility to enter a decimal number.
			 * However, in case you enter no decimals, it is most convenient to remove
			 * the trailing ".0".
			 */
      if (integer) {
        val point = valueField.text.indexOf('.')
        if (point < 0) {
          valueField.text
        } else {
          valueField.text.dropRight(valueField.text.length - point)
        }
      } else if (valueField.text.endsWith(".0")) {
        valueField.text.dropRight(2)
      } else {
        valueField.text
      }
    }

    if (mandatory || !IsZeroOrEmpty(valueField.text)) {
      val percentageSign = if (forcedPercentage || (percentageOption && percentageField.selected)) "%" else ""
      if (allowDelta) {
        deltaField.Get + valueMadeEasy + decoration.Get + percentageSign + postFix
      } else {
        valueMadeEasy + decoration.Get + percentageSign + postFix
      }
    } else {
      ""
    }
  }

  def setDefaultValue(v: Float) { defaultValue = v }
}
/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import textcompose.core._
import textcompose.editor._

/** Numbers (integer or float) with or without decoration.
  * 
  * @allowDelta flag to specify that +/- should be interpreted as +/- on existing value.
  * Used for 'size' tag. E.g. 12 means font size 12. 150% means 150% of existing font size.
  * +50% means add 50% to existing font size: multiply by 1.5. -50% means divide by 1.5.
  * This interpretation of - has the advantage that +X% and -X% together cancel out.
  * @forcedPercentage flag to specify that syntax has a mandatory (decoration) '%'.
  * Mostly used as a trick to give a unique parsing when such fields are optional.
  */
class NumberType(
  tagName: String,
  label: String,
  allowDelta: Boolean, // Only for the size tag! +5% != 5% (one adds 5% to font size, the other sets it to 5% of the current font size)
  integer: Boolean,
  decor: List[String]) extends ParameterType {

  val percentageOption = decor.length == 2 && decor(0) == "" && decor(1) == "%"
  val hasFixedDecoration = decor.length == 1

  def this(tagName: String, label: String) = {
    this(tagName, label, false, false, List())
  }
  def this(tagName: String, label: String, integer: Boolean) = {
    this(tagName, label, false, integer, List())
  }
  def this(tagName: String, label: String, decor: List[String]) = {
    this(tagName, label, false, false, decor)
  }

  private val labelLabel = new Label {
    horizontalAlignment = Alignment.Right
    foreground = Colors.standard
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
    text = "%"
    foreground = Colors.standard
  }

  private var defaultValue = 0f
  private var defaultDecor = ""

  val useDecor = !decor.isEmpty && !percentageOption && !hasFixedDecoration
  var columns = 1
  if (label != "") { columns += 1 }
  if (allowDelta) { columns += 1 }
  if (useDecor) { columns += 1 }
  if (percentageOption) { columns += 2 }

  private val layoutGroup = new GridPanel(1, columns) {
    if (label != "") { contents += labelLabel }
    if (allowDelta) { contents += deltaField.panel }
    contents += valueField
    if (useDecor) { contents += decoration.panel }
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

  def set(dn: DecoratedNumber) {
    if (allowDelta && dn.isDelta) {
      val sign = if (dn.value > 0) "+" else if (dn.value < 0) "-" else ""
      deltaField.Set(ArrayBuffer(sign), 0)
    }

    val valueForText = if (allowDelta) dn.value.abs else dn.value
    valueField.text = if (integer) valueForText.toInt.toString else valueForText.toString

    if (useDecor && decor.contains(dn.decoration)) {
      decoration.Set(ArrayBuffer(dn.decoration), 0)
    }
    percentageField.selected = percentageOption && dn.decoration == "%"
  }
  def set(i: Int) {
    valueField.text = i.toString
  }
  def set(f: Float) {
    valueField.text = f.toString
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

  private def isEmptyOrDefault: Boolean = {
    if (valueField.text == "") {
      true
    } else {
      try {
        val x = valueField.text.toFloat - defaultValue
        return x.abs < 0.0000001f && decoration.Get == defaultDecor
      } catch {
        case e: Exception => return true
      }
    }
  }

  def getUnwrapped: String = {
    
    def getValue = {
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

    if (mandatory || !isEmptyOrDefault) {
      val fixedDecoration = if (hasFixedDecoration || (percentageOption && percentageField.selected)) decor.last else ""
      if (allowDelta) {
        deltaField.Get + getValue + decoration.Get + fixedDecoration + postFix
      } else {
        getValue + decoration.Get + fixedDecoration + postFix
      }
    } else {
      ""
    }
  }

  def Get: String = getUnwrapped

  def setDefaultValue(v: Float) {
    defaultValue = v
    defaultDecor = ""
  }
  
  def setDefaultValue(dn: DecoratedNumber) {
    defaultValue = dn.value
    defaultDecor = dn.decoration
  }
}
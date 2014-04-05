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
import java.awt.Component
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import writesetter.editor.Colors

class ComboBoxType(label: String,
  values: List[String],
  isMandatory: Boolean) extends ParameterType {

  private var lastValueSwitches = false // e.g. "Custom..." pagesize.
  private var defaultValue = ""
  mandatory = isMandatory

  private var availableValues = values
  if (!mandatory) { availableValues = "" :: values }
  private val lastIndex = availableValues.length - 1

  val field = new ComboBox(availableValues)
  field.peer.setAlignmentX(Component.LEFT_ALIGNMENT)

  if (label == "") {
    AddToPanel(field, false)
  } else {
    val labeledField = new GridPanel(1, 2) {
      contents += new Label {
        horizontalAlignment = Alignment.Right
        text = label
        foreground = Colors.standard
      }
      contents += field
    }
    AddToPanel(labeledField, true)
  }

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(field.keys)
    panel.reactions += {
      case KeyPressed(`field`, Enter, _, _) => action.apply()
    }
  }

  def set(option: String): Int = {
    if (values.contains(option)) {
      field.peer.setSelectedIndex(availableValues.indexOf(option))
      1
    } else {
      0
    }
  }
  
  // FIXME: not so easy to get rid off. Used in ColorType and NumberType.
  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    // Returns the number of parameters that were read.
    val numberOfParameters = parameters.length
    if (numberOfParameters >= 1) {
      val value = parameters.slice(offset, offset + 1).mkString("", " ", "")
      if (values.contains(value)) {
        field.peer.setSelectedIndex(availableValues.indexOf(value))
        1
      } else {
        if (lastValueSwitches) {
          field.peer.setSelectedIndex(lastIndex)
          0
        } else {
          field.peer.setSelectedIndex(0)
          1
        }
      }
    } else {
      if (values.contains(defaultValue)) {
        field.peer.setSelectedIndex(availableValues.indexOf(defaultValue)) // E.g. to set page size A4.
      }
      0
    }
  }

  def grabFocus { field.peer.grabFocus }

  def IsValid = true

  def SetLastValueSwitches { lastValueSwitches = true }

  def setDefaultValue(v: String) { defaultValue = v }

  override def getUnwrapped: String = {
    val index = field.peer.getSelectedIndex
    if (lastValueSwitches && index == lastIndex) {
      ""
    } else {
      availableValues(index)
    }
  }
  
  def Get: String = Wrap(getUnwrapped)

}
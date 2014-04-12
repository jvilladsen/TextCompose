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
import writesetter.storage


class ComboBoxType(
    label: String,
    values: List[String],
    isMandatory: Boolean,
    isFontName: Boolean) extends ParameterType {

  def this(
    label: String,
    values: List[String],
    isMandatory: Boolean) = this(label, values, isMandatory, false)
  
  private var defaultValue = ""
  mandatory = isMandatory

  private var availableValues = values
  if (!mandatory) { availableValues = "" :: values }
  private val lastIndex = availableValues.length - 1
  
  var optionMapping: String => String = x => x
  def setOptionMapping(m: String => String) { optionMapping = m }

  val field = if (isFontName) {
    new ComboBox(availableValues) {
      renderer = new ListView.AbstractRenderer[String, Label](new Label) {
        def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, fontTitle: String, index: Int) {
          if (storage.StoredFontAnalysis.hasJavaFont(fontTitle)) {
            component.font = storage.StoredFontAnalysis.getJavaFont(fontTitle).deriveFont(40f)
          } else {
            component.font = storage.GUIFonts.getStandardFont(40)
          }
          component.text = fontTitle
          component.xAlignment = Alignment.Left
          if (isSelected) {
            component.border = Swing.LineBorder(list.selectionBackground, 3)
          } else {
            component.border = Swing.EmptyBorder(3)
          }
        }
      }
    }
  } else {
    new ComboBox(availableValues)
  }
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
    val index = values.map(optionMapping).indexOf(option) + (if (mandatory) 0 else 1)
    if (mandatory && index >= 0 || !mandatory && index > 0) {
      field.peer.setSelectedIndex(index)
      1
    } else {
      0
    }
  }
  
  // FIXME: not so easy to get rid off. Used in NumberType.
  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    // Returns the number of parameters that were read.
    val numberOfParameters = parameters.length
    if (numberOfParameters >= 1) {
      val value = parameters.slice(offset, offset + 1).mkString("", " ", "")
      if (values.contains(value)) {
        field.peer.setSelectedIndex(availableValues.indexOf(value))
        1
      } else {
        field.peer.setSelectedIndex(0)
        1
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

  def setDefaultValue(v: String) { defaultValue = v }

  override def getUnwrapped: String = {
    val index = field.peer.getSelectedIndex
    if (index >= 0) {
      optionMapping(availableValues(index))
    } else {
      ""
    }
  }
  
  def Get: String = Wrap(getUnwrapped)

}
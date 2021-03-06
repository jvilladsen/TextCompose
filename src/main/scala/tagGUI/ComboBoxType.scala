/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.swing._
import java.awt.Component
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import textcompose.editor.Colors
import textcompose.storage


class ComboBoxType(
    label: String,
    values: List[String],
    isMandatory: Boolean,
    isFontName: Boolean,
    useFontName: String) extends ParameterType {

  def this(
    label: String,
    values: List[String],
    isMandatory: Boolean) = this(label, values, isMandatory, false, "")
  
  private var defaultValue = ""
  mandatory = isMandatory

  private var availableValues = values
  if (!mandatory) { availableValues = "" :: values }
  private val lastIndex = availableValues.length - 1
  
  var optionMapping: String => String = x => x
  def setOptionMapping(m: String => String) { optionMapping = m }

  val field = if (isFontName || useFontName != "") {
    new ComboBox(availableValues) {
      renderer = new ListView.AbstractRenderer[String, Label](new Label) {
        def configure(list: ListView[_], isSelected: Boolean, focused: Boolean, displayText: String, index: Int) {
          val displayFont = if (useFontName != "") useFontName else displayText
          if (storage.StoredFontAnalysis.hasJavaFont(displayFont)) {
            component.font = storage.StoredFontAnalysis.getJavaFont(displayFont).deriveFont(40f)
          } else {
            component.font = storage.GUIFonts.getStandardFont(40)
          }
          component.text = displayText
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
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

import scala.collection.mutable.HashMap
import scala.swing._
import java.awt.Component
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import writesetter.{ editor, modals, storage }

class FontType extends ParameterType {

  // Combo box showing all fonts - name of font displaying in the font itself, hence the renderer.

  private val fontList = storage.StoredFontAnalysis.getAllFontTitles
  private val fontField = new ComboBox(fontList) {
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
  fontField.peer.setAlignmentX(Component.LEFT_ALIGNMENT)

  private var currentFontEncodings = List("")
  private var codePageNumberToEncodingString = new HashMap[String, String]

  private val codePageField = new ComboBox(currentFontEncodings)
  private val codePageLabeled = new GridPanel(1, 2) {
    contents += new Label {
      horizontalAlignment = Alignment.Right
      text = "Code page"
      foreground = editor.Colors.standard
    }
    contents += codePageField
  }
  private val localField = new CheckBox {
    horizontalAlignment = Alignment.Right
  }
  val localFieldLabeled = new GridPanel(1, 2) {
    contents += localField
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = "local"
      foreground = editor.Colors.standard
    }
  }

  var oldSelectedFont = ""
  var updateEncodings = new java.awt.event.ActionListener() {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      val selectedFont = fontList(fontField.peer.getSelectedIndex)
      if (selectedFont != oldSelectedFont) {
        currentFontEncodings = "" :: storage.StoredFontAnalysis.getEncodingsOfFont(fontList(fontField.peer.getSelectedIndex))
        codePageField.peer.setModel(ComboBox.newConstantModel(currentFontEncodings))
        codePageNumberToEncodingString.clear()
        for (encoding <- currentFontEncodings) {
          val key = getNumberInEncodingString(encoding)
          if (key != "") codePageNumberToEncodingString(key) = encoding
        }
        oldSelectedFont = selectedFont
      }
    }
  }
  fontField.peer.addActionListener(updateEncodings)

  private val fontInfoAction = new Action("Font info") {
    enabled = true
    def apply() {
      val selectedFontName = fontList(fontField.peer.getSelectedIndex)
      val dialog = new modals.FontInfoDialog(selectedFontName)
    }
  }
  private val fontInfoButton = new Button { action = fontInfoAction }
  fontInfoButton.peer.setAlignmentX(Component.LEFT_ALIGNMENT)

  AddToPanel(fontField, false)
  AddToPanel(codePageLabeled, true)
  AddToPanel(localFieldLabeled, true)
  AddToPanel(fontInfoButton, false)

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(fontField.keys)
    panel.reactions += { case KeyPressed(`fontField`, Enter, _, _) => action.apply() }
    panel.listenTo(codePageField.keys)
    panel.reactions += { case KeyPressed(`codePageField`, Enter, _, _) => action.apply() }
    panel.listenTo(localField.keys)
    panel.reactions += { case KeyPressed(`localField`, Enter, _, _) => action.apply() }
  }

  private def getNumberInEncodingString(encoding: String): String = {
    val elem = encoding.split(" ")
    try {
      val result = elem(0)
      result.toInt
      result
    } catch {
      case e: Exception => ""
    }
  }

  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    if (parameters.length > offset) {
      var numberRead = 1
      val value = parameters(offset)
      if (fontList.contains(value)) {
        fontField.peer.setSelectedIndex(fontList.indexOf(value))
      } else {
        fontField.peer.setSelectedIndex(0)
      }
      updateEncodings
      if (parameters.length > offset + 1) {
        numberRead += 1
        val value = parameters(offset + 1)
        if (value == "local") {
          localField.selected = true
        } else if (codePageNumberToEncodingString.contains(value)) {
          val encoding = codePageNumberToEncodingString(value)
          codePageField.peer.setSelectedIndex(currentFontEncodings.indexOf(encoding))
          if (parameters.length > offset + 2) {
            numberRead += 1
            if (parameters(offset + 2) == "local") localField.selected = true
          }
        }
      }
      numberRead
    } else {
      fontField.peer.setSelectedIndex(0)
      0
    }
  }

  def grabFocus { fontField.peer.grabFocus }

  def IsValid = true

  def Get: String = {
    var result = Wrap(fontList(fontField.peer.getSelectedIndex))
    if (codePageField.peer.getSelectedIndex > 0) {
      result += " " + getNumberInEncodingString(currentFontEncodings(codePageField.peer.getSelectedIndex))
    }
    if (localField.selected) result += " local"
    result
  }
}
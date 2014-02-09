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

package writesetter.modals

import scala.swing._
import scala.swing.GridBagPanel._
import scala.swing.Alignment
import java.awt.Font
import event._
import writesetter.{ editor, storage }

class LaidOutPanel(cols: Int, spaced: Boolean) extends GridBagPanel {

  border = Swing.EmptyBorder(20, 20, 20, 20) // top, left, bottom, right
  background = editor.Colors.modalWindows

  var currentFont = storage.GUIFonts.getStandardFont(14)

  val columns = cols
  var x = 0
  var y = 0

  def nextColumn() { x += 1 }

  def nextRow() { y += 1; x = 0 }

  def lastColumn() { x = columns - 1 }

  def add(cm: Component, flag: String) {
    cm.font = currentFont
    val c = new Constraints
    val span = if (flag == "FULL") columns else 1
    // http://www.scala-lang.org/api/current/scala/swing/GridBagPanel$Constraints.html
    c.gridx = x
    c.gridy = y
    c.gridwidth = span
    if (flag == "FULL") {
      c.fill = Fill.Both
      c.weighty = 80
      c.weightx = 7
    } else {
      if (flag != "BUTTONS") c.fill = Fill.Horizontal
      c.weighty = 0
      if (x == 0) c.weightx = 0 else c.weightx = 7
    }
    if (flag == "BUTTONS") c.anchor = Anchor.East
    if (spaced) c.weighty = 0.001f
    layout(cm) = c
    if (x + span >= columns) nextRow else nextColumn
  }

  def emptyRow() {
    nextRow
    add(new Label { text = " " }, "")
    nextRow
  }

  def field(cm: Component, label: String, labelFirst: Boolean) {
    cm.font = currentFont
    val lb = new Label {
      horizontalAlignment = Alignment.Left
      font = currentFont
      text = label
    }
    val field = new BoxPanel(Orientation.Horizontal) {
      background = editor.Colors.modalWindows
      if (labelFirst) {
        contents += lb
        contents += cm
      } else {
        contents += cm
        contents += lb
      }
    }
    add(field, "")
    nextRow
  }

  def label(lb: String, align: String, width: Int) {
    val alignment = align match {
      case "left"   => Alignment.Left
      case "center" => Alignment.Center
      case "right"  => Alignment.Right
    }
    val wrapped = if (width == 0) {
      lb
    } else {
      "<html><div style=\"width: " + width.toString + "px; \">" + lb + "</div></html>"
    }
    add(new Label { text = wrapped; horizontalAlignment = alignment }, "")
  }

  def twoButtons(okAction: Action, cancelAction: Action) {
    val okButton = new Button {
      tooltip = "Alt-Enter"
      mnemonic = Key.Enter
      action = okAction
    }
    val cancelButton = new Button {
      tooltip = "Alt-Esc"
      mnemonic = Key.Escape
      action = cancelAction
    }
    val buttonPanel = new BoxPanel(Orientation.Horizontal) {
      background = editor.Colors.modalWindows
      contents += cancelButton
      contents += okButton
    }
    emptyRow
    lastColumn
    add(buttonPanel, "BUTTONS")
  }

  def setFont(fontName: String, fontSize: Int) {
    try {
      currentFont = new Font(fontName, Font.PLAIN, fontSize)
    } catch {
      case e: Exception => editor.DialogBox.stackTrace("Could not get font '" + fontName + "' size " + fontSize.toString + ": " + e.getMessage, e)
    }
  }
}
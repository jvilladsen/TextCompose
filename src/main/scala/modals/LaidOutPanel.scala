/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.modals

import scala.swing._
import scala.swing.GridBagPanel._
import scala.swing.Alignment
import java.awt.Font
import event._
import textcompose.{ editor, storage }

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
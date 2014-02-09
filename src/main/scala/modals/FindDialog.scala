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
import event._
import java.awt.Font
import Key._

class FindDialog(
  textEditor: writesetter.editor.TextFileEditor,
  initialFindText: String,
  initialCaseSensitive: Boolean) extends Dialog {

  private var confirmed = false

  private val findText = new TextField {
    text = initialFindText
    minimumSize = new Dimension(250, 25)
    maximumSize = new Dimension(500, 25)
  }
  private val caseSensitive = new CheckBox {
    horizontalAlignment = Alignment.Right
    selected = initialCaseSensitive
  }
  listenTo(findText.keys)
  reactions += {
    case KeyPressed(`findText`, Enter, _, _)  => okAction.apply()
    case KeyPressed(`findText`, Escape, _, _) => cancelAction.apply()
  }

  val okAction = new Action("Find") {
    enabled = true
    def apply() {
      confirmed = true
      textEditor.findNext(findText.text, false, caseSensitive.selected)
      findText.peer.grabFocus
    }
  }
  val cancelAction = new Action("Close") {
    enabled = true
    def apply() {
      close
      dispose
    }
  }

  lazy val panel = new LaidOutPanel(1, false)

  panel.field(findText, "Find:", true)
  panel.field(caseSensitive, "Case sensitive", false)
  panel.twoButtons(okAction, cancelAction)

  contents = panel

  title = "Find"
  modal = true
  resizable = false
  preferredSize = new Dimension(350, 130)
  // peer.getRootPane().putClientProperty("Window.alpha", 0.85f)	// transparency (OS dependent?)
  centerOnScreen
  pack
  open

  def getConfirmation: Boolean = confirmed
  def getFindText = findText.text
  def getCaseSensitive = caseSensitive.selected
}
/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.modals

import scala.swing._
import scala.swing.GridBagPanel._
import event._
import java.awt.Font
import Key._

class FindDialog(
  textEditor: textcompose.editor.TextFileEditor,
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
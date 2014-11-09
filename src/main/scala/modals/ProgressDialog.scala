/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.modals

import scala.swing._
import textcompose.editor.Colors

class ProgressDialog(dialogTitle: String) extends Dialog {

  private val bar = new textcompose.editor.Progress(true)

  def makeVisible() {
    pack()
    open()
    visible = true
  }

  def update(p: Float, m: String) {
    bar.update(p, m)
  }

  def finish() {
    close
    dispose
  }

  contents = bar
  title = dialogTitle
  // modal = true
  resizable = false
  preferredSize = new Dimension(350, 130)
  centerOnScreen()
  // We set the background color to avoid getting a brief white flash when the window is opened.
  background = Colors.modalWindows
}
/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import swing.ProgressBar
import swing.GridBagPanel
import swing.Label
import swing._
import scala.swing.GridBagPanel._
import concurrent.ExecutionContext.Implicits.global
import concurrent._

class Progress(showLabel: Boolean) extends GridBagPanel {

  private var percentage = 0
  private var message = ""
  private val bar = new ProgressBar { value = percentage }
  private val label = new Label { text = message }

  val c = new Constraints
  c.gridx = 1
  c.gridy = 1
  c.gridwidth = 1
  c.gridheight = 2
  c.fill = Fill.Horizontal
  c.weightx = 0.001f
  c.weighty = 0.001f
  
  if (showLabel) layout(label) = c
  layout(bar) = c

  border = Swing.EmptyBorder(2, 10, 2, 10)

  // If the progress bar is not rendered you probably need to run the action in a future.
  
  def update(p: Float) {
    percentage = p.toInt
    future {
      bar.value = percentage
    }
  }

  def update(p: Float, m: String) {
    percentage = p.toInt
    message = m
    future {
      bar.value = percentage
      label.text = message
    }
  }
}
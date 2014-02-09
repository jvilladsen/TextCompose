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
import java.awt.Font
import event._
import Key._
import writesetter.{ editor, storage }

class TemplateChooser extends Dialog {

  private var selectedTemplate = ""
  private var selectedIndex = 0
  private var completed = true
  private val noTemplate = "none"

  private def makeButton(title: String): RadioButton = {
    val button = new RadioButton(title) {
      font = storage.GUIFonts.getStandardFont(18)
    }
    listenTo(button.keys)
    reactions += {
      case KeyPressed(`button`, Enter, _, _) => {
        close
        dispose
      }
      case KeyPressed(`button`, Escape, _, _) => {
        completed = false
        close
        dispose
      }
      case KeyPressed(`button`, Up, _, _) => {
        indexSelected(-1)
      }
      case KeyPressed(`button`, Down, _, _) => {
        indexSelected(+1)
      }
    }
    listenTo(button)
    reactions += {
      case ButtonClicked(`button`) => titleSelected(title)
    }
    button
  }

  val options = List(noTemplate) ++ storage.Configurations.GetListOfTemplates
  val optionButtons = options.map(makeButton)

  private def titleSelected(title: String) {
    selectedTemplate = title
    var index = 0
    for (t <- optionButtons) {
      if (t.text == title) {
        t.selected = true
        t.focusPainted = true
        selectedIndex = index
      } else {
        t.selected = false
        t.focusPainted = false
      }
      index += 1
    }
  }

  private def indexSelected(delta: Int) {
    selectedIndex += delta
    val length = options.length
    if (selectedIndex < 0) {
      selectedIndex = length - 1
    } else if (selectedIndex >= length) {
      selectedIndex = 0
    }
    titleSelected(options(selectedIndex))
  }

  lazy val buttonPanel = new BoxPanel(Orientation.Vertical) {
    contents ++= optionButtons
    background = editor.Colors.modalWindows
  }
  lazy val panel = new LaidOutPanel(1, false)
  panel.add(buttonPanel, "")
  contents = panel

  indexSelected(0)
  title = "Choose template"
  modal = true
  resizable = false
  minimumSize = new Dimension(300, 30)
  centerOnScreen
  pack
  open

  def isCompleted = completed
  def isTemplate = selectedTemplate != noTemplate
  def getTemplate = selectedTemplate
}
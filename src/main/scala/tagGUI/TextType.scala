/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import textcompose.editor.Colors

class TextType(label: String, large: Boolean) extends ParameterType {

  private val field = new TextField {
    horizontalAlignment = Alignment.Left
    columns = 13
  }

  if (label == "") {
    val unLabeledField = new GridPanel(1, 1) {
      contents += field
    }
    AddToPanel(unLabeledField, true)
  } else {
    if (large) {
      val labeledField = new BoxPanel(Orientation.Vertical) {
        contents += new Label {
          horizontalAlignment = Alignment.Left
          text = label
          foreground = Colors.standard
        }
        contents += field
      }
      AddToPanel(labeledField, true)
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
  }

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(field.keys)
    panel.reactions += {
      case KeyPressed(`field`, Enter, _, _) => action.apply()
    }
  }

  def set(s: String) { field.text = s }

  def grabFocus { field.peer.grabFocus }

  def IsValid = true

  def IsEmpty = field.text == ""

  def getUnwrapped: String = field.text
  
  def Get = Wrap(field.text)
}
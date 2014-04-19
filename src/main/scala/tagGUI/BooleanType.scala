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

class BooleanType(representation: String, label: String) extends ParameterType {

  private val field = new CheckBox {
    horizontalAlignment = Alignment.Right
  }

  val labeledField = new GridPanel(1, 2) {
    contents += field
    contents += new Label {
      horizontalAlignment = Alignment.Left
      text = label
      foreground = Colors.standard
    }
  }
  AddToPanel(labeledField, true)

  setNotMandatory()

  def GetRepresentation = representation

  def SetDirectly(selected: Boolean) { field.selected = selected }

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(field.keys)
    panel.reactions += {
      case KeyPressed(`field`, Enter, _, _) => action.apply()
    }
  }

  def set() { field.selected = true }

  def grabFocus { field.peer.grabFocus }

  def IsValid = true

  // def IsEmpty = !field.selected

  def getUnwrapped: String = if (field.selected) { representation } else { "" }
  
  def Get = Wrap(getUnwrapped)
}
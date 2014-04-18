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
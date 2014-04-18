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
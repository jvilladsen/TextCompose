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

package writesetter.tagGUI

import scala.swing._
import java.awt.Component
import scala.collection.mutable.ArrayBuffer
import writesetter.editor.Colors

abstract class ParameterType {

  val panel = new BoxPanel(Orientation.Vertical) {
    background = Colors.supportPane
  }

  var mandatory = true
  def setNotMandatory() { mandatory = false }

  // Post fix used from the cell tag.
  var postFix = ""
  def SetPostFix(pf: String) { postFix = pf }

  def LeftAlign(c: scala.swing.Component) {
    c.peer.setAlignmentX(Component.LEFT_ALIGNMENT)
  }

  def RightAlign(c: scala.swing.Component) {
    c.peer.setAlignmentX(Component.RIGHT_ALIGNMENT)
  }

  def AddActionOnEnter(action: Action) {}

  def AddToPanel(c: scala.swing.Component, setBackground: Boolean) {
    LeftAlign(c)
    if (setBackground) { c.background = Colors.supportPane }
    panel.contents += c
  }

  def grabFocus

  def IsValid: Boolean

  def Wrap(s: String): String = {
    if (s.contains(" ") || s == "" && mandatory) {
      "\"" + s + "\""
    } else {
      s
    }
  }

  def getUnwrapped: String
  
  def Get: String
}
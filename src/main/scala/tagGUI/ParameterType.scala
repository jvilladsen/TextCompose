/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import scala.swing._
import java.awt.Component
import scala.collection.mutable.ArrayBuffer
import textcompose.editor.Colors

abstract class ParameterType {

  val panel = new BoxPanel(Orientation.Vertical) {
    background = Colors.supportPane
  }

  var mandatory = true
  def setNotMandatory() { mandatory = false }

  var actions = new ArrayBuffer[TagAction]

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

  def setActions(a: ArrayBuffer[TagAction]) { actions = a }

  def setOffset(i: Int) {
    for (a <- actions) { a.setFieldOffset(i) }
  }
  
  def setFields(f: ArrayBuffer[ParameterType]) {
    for (a <- actions) {
      a.prepareFromDialog(f, panel.peer)
    }
  }
  
  def addActionButtons() {
    if (!actions.isEmpty) {
      val buttonPanel = new BoxPanel(Orientation.Vertical) {
        for (a <- actions) {
          val button = new Button(a)
          button.peer.setAlignmentX(Component.LEFT_ALIGNMENT)
          contents += button
        }
      }
      AddToPanel(buttonPanel, true)
    }
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
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

package writesetter.editor

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

class TagPane {

  private var triggeredFromTagTree = false
  private var fileKey = ""
  private var isInsideTag = false
  var foundTagStartingAt = 0
  var foundTagEndingAt = 0
  var result = ""

  val panel = new BoxPanel(Orientation.Vertical) {
    background = Colors.supportPane
    border = Swing.EmptyBorder(1, 5, 1, 1)
    minimumSize = new Dimension(170, 200)
  }
  private var dialog: writesetter.tagGUI.TagDialog = null

  val fakeAction = new Action("<signal to update editor>") {
    enabled = false
    def apply() { None }
  }

  private def signalUpdate() {
    // Workspace listens to changes in this "fake action".
    fakeAction.enabled = !fakeAction.enabled // toggle to trigger an update of editor (hack)
  }

  private def clearLayout() { panel.contents.clear() }

  private def addContent(c: scala.swing.Component) { panel.contents += c }

  private def addSyntaxSelector(syntaxes: List[String], currentSyntax: Int) {
    val syntaxAlternatives =
      new writesetter.tagGUI.ComboBoxType("form", syntaxes, true)

    syntaxAlternatives.field.peer.setSelectedIndex(currentSyntax)

    val updateDialogFromSelf = new java.awt.event.ActionListener() {
      def actionPerformed(event: java.awt.event.ActionEvent) {
        val forcedSyntax = syntaxAlternatives.field.peer.getSelectedIndex
        refreshLayout(dialog.getAsSourceElement, forcedSyntax)
      }
    }
    syntaxAlternatives.field.peer.addActionListener(updateDialogFromSelf)

    addContent(syntaxAlternatives.panel)
  }

  private def assembleDialog(se: writesetter.core.SourceElement, forcedSyntax: Int) {

    dialog = new writesetter.tagGUI.TagDialog(fileKey, se.TagName)
    val okAction = new Action("OK") {
      enabled = true
      def apply() {
        if (dialog.IsValid) {
          result = dialog.Get
          signalUpdate()
        }
      }
    }
    dialog.layout(se, okAction, updateDialogFromSelf, forcedSyntax)

    if (dialog.IsKnownTag) {
      val par = dialog.preprocessParameters(se.TagName, se.Parameters)
      addContent(dialog.panel)
      
      val multipleSyntaxes = dialog.syntaxes.length > 1
      if (dialog.HasParameters || multipleSyntaxes) {
        addContent(new Button(okAction))
      }
      if (multipleSyntaxes) {
        addSyntaxSelector(dialog.syntaxes.toList, dialog.currentSyntax)
      }
      panel.contents += Swing.VStrut(10000) // is there a nicer way to pack the content from the top?
      if (triggeredFromTagTree) dialog.grabFocus
    } else {
      addContent(dialog.panel)
    }
  }

  private def refreshLayout(se: writesetter.core.SourceElement, forcedSyntax: Int) {
    clearLayout()
    if (isInsideTag) { assembleDialog(se, forcedSyntax) }
    panel.revalidate()
    panel.repaint()
    triggeredFromTagTree = false
  }

  def updateColors() {
    panel.background = Colors.supportPane
    refreshLayout(dialog.getAsSourceElement, -1)
  }

  /** Build the tag dialog based on data in the source code around the caret.
    *  
    * This is triggered upon change of position of the caret in the text editor
    * in a future with a 600ms delay.
    */
  def updateFromEditor(
    givenKey: String,
    inside: Boolean,
    start: Int,
    end: Int,
    se: writesetter.core.SourceElement) {

    fileKey = givenKey
    isInsideTag = inside
    foundTagStartingAt = start
    foundTagEndingAt = end

    refreshLayout(se, -1)
  }

  /** Rebuild the tag dialog based on data in the tag dialog itself.
    *  
    * If, for example, you change 'color system' in 'color' tag from RGB to HSL,
    * then the dialog is rebuilt to update the next three labels, 'red' to 'hue' etc.
    * Other, example: different fonts have different lists of available code pages,
    * so a change of font should trigger re-assignment of the code page combo-box.
    */
  val updateDialogFromSelf = new java.awt.event.ActionListener() {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      refreshLayout(dialog.getAsSourceElement, -1)
    }
  }

  def updateWithParserErrorFromEditor(message: String) {
    clearLayout()

    dialog = new writesetter.tagGUI.TagDialog(fileKey, "")
    dialog.layoutParserError(message)
    addContent(dialog.panel)

    panel.revalidate()
    panel.repaint()
    triggeredFromTagTree = false
  }

  def setTriggeredFromTagTree() { triggeredFromTagTree = true }

}
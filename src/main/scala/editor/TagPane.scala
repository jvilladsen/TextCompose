/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import scala.swing._
import scala.collection.mutable.ArrayBuffer
import java.beans.{ PropertyChangeEvent, PropertyChangeListener }

class TagPane {

  private var doGrabFocus = false
  private var fileKey = ""
  private var isInsideTag = false
  var foundTagStartingAt = 0
  var foundTagEndingAt = 0
  var result = ""
  
  var latestForcedSyntax = -1

  val panel = new BoxPanel(Orientation.Vertical) {
    background = Colors.supportPane
    border = Swing.EmptyBorder(1, 5, 1, 1)
    minimumSize = new Dimension(170, 200)
  }
  private var dialog: textcompose.tagGUI.TagDialog = null

  val fakeAction = new Action("<signal to update editor>") {
    enabled = false
    def apply() { None }
  }

  private def signalUpdate() {
    fakeAction.enabled = !fakeAction.enabled
  }

  private def clearLayout() { panel.contents.clear() }

  private def addContent(c: scala.swing.Component) { panel.contents += c }

  private def addSyntaxSelector(syntaxes: List[String], currentSyntax: Int) {
    val syntaxAlternatives =
      new textcompose.tagGUI.ComboBoxType("form", syntaxes, true)

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

  private def assembleDialog(se: textcompose.core.SourceElement, forcedSyntax: Int) {

    dialog = new textcompose.tagGUI.TagDialog(fileKey, se.TagName)
    val okAction = new Action("OK") {
      enabled = true
      def apply() {
        if (dialog.IsValid) {
          result = dialog.Get
          signalUpdate()
        }
      }
    }

    try {
      dialog.layout(se, okAction, updateDialogFromSelf, forcedSyntax)
    } catch {
      case e: Exception => DialogBox.stackTrace("Failed laying out tag dialog", e)
    }

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
      if (doGrabFocus) dialog.grabFocus
    } else {
      addContent(dialog.panel)
    }
  }

  private def refreshLayout(se: textcompose.core.SourceElement, forcedSyntax: Int) {
    clearLayout()
    if (isInsideTag) { assembleDialog(se, forcedSyntax) }
    panel.revalidate()
    panel.repaint()
    doGrabFocus = false
    latestForcedSyntax = forcedSyntax
  }

  def updateColors() {
    panel.background = Colors.supportPane
    try {
      refreshLayout(dialog.getAsSourceElement, -1)
    } catch {
      case e: Exception => None // we may have no dialog
    }
  }

  /**
    * Build the tag dialog based on data in the source code around the caret.
    *
    * This is triggered upon change of position of the caret in the text editor
    * in a future with a 600ms delay.
    */
  def updateFromEditor(
    givenKey: String,
    inside: Boolean,
    start: Int,
    end: Int,
    se: textcompose.core.SourceElement) {

    fileKey = givenKey
    isInsideTag = inside
    foundTagStartingAt = start
    foundTagEndingAt = end

    refreshLayout(se, -1)
  }

  /**
    * Rebuild the tag dialog based on data in the tag dialog itself.
    *
    * If, for example, you change 'color system' in 'color' tag from RGB to HSL,
    * then the dialog is rebuilt to update the next three labels, 'red' to 'hue' etc.
    * Other, example: different fonts have different lists of available code pages,
    * so a change of font should trigger re-assignment of the code page combo-box.
    */
  val updateDialogFromSelf = new java.awt.event.ActionListener() {
    def actionPerformed(event: java.awt.event.ActionEvent) {
      doGrabFocus = true
      refreshLayout(dialog.getAsSourceElement, latestForcedSyntax)
    }
  }

  def updateWithParserErrorFromEditor(message: String) {
    clearLayout()

    dialog = new textcompose.tagGUI.TagDialog(fileKey, "")
    dialog.layoutParserError(message)
    addContent(dialog.panel)

    panel.revalidate()
    panel.repaint()
    doGrabFocus = false
  }

  def setGrabFocus() { doGrabFocus = true }

}
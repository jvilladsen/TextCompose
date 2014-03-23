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

  var panel = new BoxPanel(Orientation.Vertical) {
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

  private def assembleDialog(se: writesetter.core.SourceElement) {

    dialog = new writesetter.tagGUI.TagDialog(fileKey, panel.peer, se.TagName)
    val okAction = new Action("OK") {
      enabled = true
      def apply() {
        if (dialog.IsValid) {
          result = dialog.Get
          signalUpdate()
        }
      }
    }
    dialog.Layout(se, okAction)

    if (dialog.IsKnownTag) {
      val par = dialog.preprocessParameters(se.TagName, se.Parameters)
      dialog.Set(par, 0)
      addContent(dialog.panel)
      if (dialog.HasParameters) { addContent(new Button(okAction)) }
      panel.contents += Swing.VStrut(10000) // is there a nicer way to pack the content from the top?
      if (triggeredFromTagTree) dialog.grabFocus

      // Here, we can set up a listener on some fake action inside dialog object - listener calls refreshLayout
      val setSwitcherAndRefresh = new PropertyChangeListener() {
        def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
          val newSelectedValue = dialog.GetSwitchingSelectedValue
          if (par.length > 0) {
            par(0) = newSelectedValue
          } else {
            par.append(newSelectedValue)
          }
          refreshLayout(se)
        }
      }
      dialog.fakeAction.peer.addPropertyChangeListener(setSwitcherAndRefresh)
    } else {
      addContent(dialog.panel)
    }
  }

  private def refreshLayout(se: writesetter.core.SourceElement) {
    clearLayout()
    if (isInsideTag) { assembleDialog(se) }
    panel.revalidate()
    panel.repaint()
    triggeredFromTagTree = false
  }

  def updateColors() {
    panel.background = Colors.supportPane
    if (dialog != null) {
      dialog.signalUpdate() // instead of updating color on each single label, field, panel...
    }
  }

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

    refreshLayout(se)
  }

  // FIXME: apart from presenting simple parser errors we could also parse tags and show error from that.
  def updateWithParserErrorFromEditor(message: String) {
    clearLayout()

    dialog = new writesetter.tagGUI.TagDialog(fileKey, panel.peer, "")
    dialog.layoutParserError(message)
    addContent(dialog.panel)

    panel.revalidate()
    panel.repaint()
    triggeredFromTagTree = false
  }

  def setTriggeredFromTagTree() { triggeredFromTagTree = true }

}
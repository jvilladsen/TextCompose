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

  def updateColors() {
    panel.background = Colors.supportPane
    if (dialog != null) {
      dialog.signalUpdate() // instead of updating color on each single label, field, panel...
    }
  }

  val fakeAction = new Action("<signal to update editor>") {
    enabled = false
    def apply() { None }
  }

  def updateFromEditor(
    givenKey: String,
    inside: Boolean,
    start: Int,
    end: Int,
    tag: writesetter.core.SourceElement) {
    fileKey = givenKey
    isInsideTag = inside
    foundTagStartingAt = start
    foundTagEndingAt = end

    refreshLayout(tag.TagName, tag.Parameters)
  }

  private def refreshLayout(tagName: String, parameters: ArrayBuffer[String]) {
    clearLayout
    if (isInsideTag) { assembleDialog(tagName, parameters) }
    panel.revalidate()
    panel.repaint()
    triggeredFromTagTree = false
  }

  private def assembleDialog(tagName: String, parameters: ArrayBuffer[String]) {

    dialog = new writesetter.tagGUI.TagDialog(fileKey, panel.peer, tagName)
    val okAction = new Action("OK") {
      enabled = true
      def apply() {
        if (dialog.IsValid) {
          result = dialog.Get
          signalUpdate
        }
      }
    }
    dialog.Layout(parameters, okAction)

    if (dialog.IsKnownTag) {
      val par = dialog.preprocessParameters(tagName, parameters)
      dialog.Set(par, 0)
      addContent(dialog.panel)
      if (dialog.HasParameters) { addOKButton(okAction) }
      panel.contents += Swing.VStrut(10000) // is there a nicer way to pack the content from the top?
      if (triggeredFromTagTree) dialog.grabFocus

      // Here, we can set up a listener on some fake action inside dialog object - listener calls refreshLayout
      var setSwitcherAndRefresh = new PropertyChangeListener() {
        def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
          val newSelectedValue = dialog.GetSwitchingSelectedValue
          if (par.length > 0) {
            par(0) = newSelectedValue
          } else {
            par.append(newSelectedValue)
          }
          refreshLayout(tagName, par)
        }
      }
      dialog.fakeAction.peer.addPropertyChangeListener(setSwitcherAndRefresh)
    } else {
      addContent(dialog.panel)
    }
  }

  def SetTriggeredFromTagTree { triggeredFromTagTree = true }

  private def signalUpdate {
    // Workspace listens to changes in this "fake action".
    fakeAction.enabled = !fakeAction.enabled // toggle to trigger an update of editor (hack)
  }

  private def clearLayout { panel.contents.clear() }

  private def addContent(c: scala.swing.Component) { panel.contents += c }

  private def addOKButton(okAction: Action) {
    val okButton = new Button {
      text = "OK"
      action = okAction
    }
    addContent(okButton)
  }
}
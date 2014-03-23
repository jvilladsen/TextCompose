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
import javax.swing.JPanel
import java.awt.{ Component, FileDialog, Frame }
import scala.collection.mutable.ArrayBuffer
import event._
import Key._
import writesetter.{editor, storage}

class FileType(title: String) extends ParameterType {

  val field = new TextField { columns = 13 }

  var isImage = false
  def SetIsImage { isImage = true }

  private def proposedDirectory: String = {
    if (storage.FileMethods.IsFile(field.text)) {
      storage.FileMethods.GetDirectory(field.text)
    } else {
      storage.Configurations.GetLatestDirectory("Chooser")
    }
  }

  private val fileChooserAction = new Action(title) {
    enabled = true
    def apply() {
      var openFileChooser = new java.awt.FileDialog(editor.Application.top.peer, "Open File", FileDialog.LOAD)
      openFileChooser.setDirectory(proposedDirectory)
      openFileChooser.setVisible(true)

      if (openFileChooser.getFile != null) {
        field.text = openFileChooser.getDirectory + openFileChooser.getFile
        storage.Configurations.updateLatestDirectory(openFileChooser.getDirectory, "Chooser")
      }
      openFileChooser.dispose
    }
  }
  private val chooserButton = new Button(fileChooserAction)
  chooserButton.peer.setAlignmentX(Component.LEFT_ALIGNMENT)

  private val fileOpenAction = new Action("Open") {
    enabled = true
    def apply() {
      if (storage.FileMethods.IsFile(field.text)) {
        try {
          if (isImage) {
            editor.DesktopInteraction.OpenPDF(field.text, false, "", 0) // fileTitle only used for preview
          } else {
            editor.Application.openNamedFile(field.text)
          }
        } catch {
          case e: Exception => {
            editor.DialogBox.error(
              "Could not open '" + field.text + "': " + e.getMessage)
          }
        }
      } else {
        editor.DialogBox.error("Unknown file '" + field.text + "'.")
      }
    }
  }
  private val openButton = new Button(fileOpenAction)
  openButton.peer.setAlignmentX(Component.LEFT_ALIGNMENT)

  private val buttonPanel = new BoxPanel(Orientation.Vertical) {
    contents += chooserButton
    contents += openButton
  }

  AddToPanel(field, false)
  AddToPanel(buttonPanel, true)

  override def AddActionOnEnter(action: Action) {
    panel.listenTo(field.keys)
    panel.reactions += {
      case KeyPressed(`field`, Enter, _, _) => action.apply()
    }
  }

  def Set(parameters: ArrayBuffer[String], offset: Int): Int = {
    if (parameters.length > offset) { field.text = parameters(offset); 1 } else { 0 }
  }

  def grabFocus { field.peer.grabFocus }

  def IsValid: Boolean = field.text != ""

  def Get: String = Wrap(field.text)
}
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

import java.awt.{ Component, FileDialog, Frame }
import textcompose.{ editor, storage }

object FileChooser extends TagAction("Choose file") {

  enabled = true

  private def proposedDirectory: String = {
    val fileName = fields(offset).getUnwrapped
    if (storage.FileMethods.IsFile(fileName)) {
      storage.FileMethods.GetDirectory(fileName)
    } else {
      storage.Configurations.GetLatestDirectory("Chooser")
    }
  }

  def apply() {
    var openFileChooser = new java.awt.FileDialog(editor.Application.top.peer, "Choose File", FileDialog.LOAD)
    openFileChooser.setDirectory(proposedDirectory)
    openFileChooser.setVisible(true)

    if (openFileChooser.getFile != null) {
      fields(offset) match {
        case f: TextType => f.set(openFileChooser.getDirectory + openFileChooser.getFile)
        case _ =>
      }
      storage.Configurations.updateLatestDirectory(openFileChooser.getDirectory, "Chooser")
    }
    openFileChooser.dispose
  }
}
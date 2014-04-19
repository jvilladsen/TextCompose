/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
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
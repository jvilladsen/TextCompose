/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.tagGUI

import textcompose.{ editor, storage }

class OpenFile(isImage: Boolean) extends TagAction("Open") {

  enabled = true

  def apply() {
    val fileName = fields(offset).getUnwrapped
    if (storage.FileMethods.IsFile(fileName)) {
      try {
        if (isImage) {
          editor.DesktopInteraction.OpenPDF(fileName, false, "", 0) // fileTitle only used for preview
        } else {
          editor.Application.openNamedFile(fileName)
        }
      } catch {
        case e: Exception => {
          editor.DialogBox.error(
            "Could not open '" + fileName + "': " + e.getMessage)
        }
      }
    } else {
      editor.DialogBox.error("Unknown file '" + fileName + "'.")
    }
  }
}

object OpenTextFile extends OpenFile(false)

object OpenImageFile extends OpenFile(true)

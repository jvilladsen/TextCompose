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

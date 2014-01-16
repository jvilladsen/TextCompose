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

package writesetter.core

import java.io.File
import scala.util.matching.Regex
import writesetter.editor

class Arguments(
  internal: Boolean,
  providedSourceName: String,
  temporaryLocation: Boolean,
  openPDFViewer: PreviewType.Value) {

  var SourceFileName = "" // The file name part of providedSourceName
  var SourceFileDirectory = "" // The path part of providedSourceName
  var sourceFullFileName = ""
  var PDFDefaultTitle = ""
  var PDFFileName = ""
  var VariablesFileName = ""

  var caretPostionForPreview = 0
  var previewPageNumber = 0

  checkAndSplitSourceFileName()
  determinePDFFileName()
  LatestExtensions.addFileName(providedSourceName)

  private def checkAndSplitSourceFileName() {
    val fileHandle = new java.io.File(providedSourceName)
    if (fileHandle.exists) {
      if (fileHandle.isDirectory) {
        editor.DialogBox.error("Please specify a file name. '" + providedSourceName + "' is a directory")
      }
      if (fileHandle.canRead) {
        SourceFileName = fileHandle.getName
        if (SourceFileName ne providedSourceName) {
          SourceFileDirectory = fileHandle.getParent + Environment.fileSeparator
        }
        sourceFullFileName = fileHandle.getAbsolutePath()

        VariablesFileName = SourceFileDirectory + "." + SourceFileName
      } else {
        editor.DialogBox.error("Cannot read file " + providedSourceName)
      }
    } else {
      editor.DialogBox.error("There is no file named '" + providedSourceName + "'")
    }
  }

  private def determinePDFFileName() {
    val fileNameWithExtension = new Regex("""(.+)\.([^.]+)""")
    SourceFileName match {
      case fileNameWithExtension(beforeExtensionPoint, afterExtensionPoint) => {
        PDFDefaultTitle = beforeExtensionPoint
        PDFFileName = SourceFileDirectory + beforeExtensionPoint + ".pdf"
      }
      case _ => {
        PDFDefaultTitle = SourceFileName
        PDFFileName = SourceFileDirectory + SourceFileName + ".pdf"
      }
    }
  }

  def pathToReachablePath(givenPath: String): String = {
    if (Environment.PathIsAbsolute(givenPath)) {
      givenPath
    } else {
      SourceFileDirectory + givenPath
    }
  }

  def isTemporaryLocation: Boolean = temporaryLocation

  def maybeLaunchPDFViewer(errorCount: Int) {
    if (openPDFViewer == PreviewType.Yes || openPDFViewer == PreviewType.IfNoErrors && errorCount == 0) {
      editor.DesktopInteraction.OpenPDF(
        PDFFileName,
        internal, // true => previewer, false => ask OS
        if (temporaryLocation) PDFDefaultTitle + ".pdf" else PDFFileName,
        previewPageNumber)
    }
  }
}

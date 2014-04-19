/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import java.io.File
import textcompose.{ editor, storage }

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
  LatestExtensions.addExtension("self")

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
    PDFDefaultTitle = storage.FileMethods.splitFileNameAtLastPeriod(SourceFileName)._1
    PDFFileName = SourceFileDirectory + PDFDefaultTitle + ".pdf"
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

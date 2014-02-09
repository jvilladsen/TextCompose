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

import swing._
import java.beans.{ PropertyChangeEvent, PropertyChangeListener }
import java.io.File
import writesetter.{ core, storage }
import writesetter.core.PreviewType

class TextFileEditor(fontSize: Int) {

  val file = new TextFile
  val editor = new TextEditor(fontSize: Int)

  def newText(number: Int): Boolean = {
    file.newText(number)
    editor.newText()
  }

  def chooseAndReadFile(forcedEncoding: String) {
    file.chooseFile(forcedEncoding)
    editor.readTextFromFile(file.fullFileName, file.fileIsReadOnly, file.encoding)
  }

  def readNamedFile(name: String) {
    file.openNamedFile(name)
    editor.readTextFromFile(file.fullFileName, file.fileIsReadOnly, file.encoding)
  }

  private def saveFile(
    saveEvenIfNotDirty: Boolean,
    updateSourcesMetaData: Boolean): Boolean = {

    var completed = true
    if (!file.fileIsReadOnly && (saveEvenIfNotDirty || editor.fileIsDirty)) {
      completed = file.checkFileStamp
      if (completed) {
        editor.writeTextToFile(file.fullFileName, true, file.encoding)
        file.updateTimeStampAfterSave()
        file.updateFromFullName()
        if (updateSourcesMetaData) {
          storage.SourcesMetaData.updateFileData(file.fullFileName, file.fileName, file.encoding, file.dictionary)
        }
      }
    }
    completed
  }

  def saveFileAs(forcedEncoding: String, moveOrRename: Boolean): Boolean = {
    var completed = false
    val originalFileName = file.fullFileName

    if (file.chooseFileForSaving(forcedEncoding, moveOrRename)) {

      completed = saveFile(true, !moveOrRename)

      if (completed && moveOrRename && originalFileName != file.fullFileName) {
        val originalFile = new File(originalFileName)
        originalFile.delete()
        storage.SourcesMetaData.renameFileData(originalFileName, file.fullFileName, file.fileName)
      }
    }
    completed
  }

  def saveOrSaveAs(forcedEncoding: String): Boolean = {
    if (file.fullFileName != "") {
      saveFile(false, true) // only save if dirty, do update sources meta data
    } else {
      saveFileAs(forcedEncoding, false)
    }
  }

  def saveOrDiscardDirtyFile(): Boolean = {
    var completed = true
    if (editor.fileIsDirty) {
      val message = "Do you want to save the changes to '" + file.fileName + "'?"
      var askTheUser = true

      while (askTheUser) {
        val answer = DialogBox.question(message)
        if (answer == "Yes") {
          completed = saveOrSaveAs("")
          askTheUser = !completed
        } else {
          // answer is No or Cancel
          completed = answer == "No"
          askTheUser = false
        }
      }
    }
    completed
  }

  def Refresh {
    if (saveOrDiscardDirtyFile()) {
      editor.readTextFromFile(file.fullFileName, file.fileIsReadOnly, file.encoding)
    }
  }

  def grabFocus { editor.grabFocus }

  def updateColors() {
    file.updateColors()
    editor.updateColors()
  }

  def updateFont(fontSize: Int) { editor.updateFont(fontSize) }

  def buildPDF() {
    var localFileName = file.getFileKey
    if (file.fullFileName == "") {
      editor.writeTextToFile(localFileName, false, "")
    }
    var args = new core.Arguments(true, // internal
      localFileName,
      file.fullFileName == "", // temporaryLocation,
      PreviewType(storage.Configurations.getViewAfterCompile))
    args.caretPostionForPreview = editor.getCursorPosition
    try {
      core.Compiler.build(args)
    } catch {
      case e: Exception => {
        val message = e.getMessage + ".\nIt may help to close any open PDF viewer."
        DialogBox.stackTrace(message, e)
      }
    }
    grabFocus
  }

  def viewPDF() {
    file.viewPDF()
    grabFocus
  }

  def showInFinder() {
    try {
      file.showInFinder()
    } catch {
      case e: Exception => {
        DialogBox.stackTrace("Could not show in file system: " + e.getMessage, e)
      }
    }
    grabFocus
  }

  def registerNewExtension() {
    core.FileRegistration.NewFile(file.fullFileName, "extension")
  }
  def registerNewTemplate() {
    core.FileRegistration.NewFile(file.fullFileName, "template")
  }

  def Undo { if (editor.undoAction.enabled) { editor.undoAction.apply } }

  def Redo { if (editor.redoAction.enabled) { editor.redoAction.apply } }

  def insertAtCurrentPosition(stringToInsert: String) {
    if (!file.fileIsReadOnly) {
      editor.document.insertString(editor.getCursorPosition, stringToInsert, null)
      grabFocus
    }
  }

  def overwriteAtGivenPosition(stringToWrite: String, start: Int, end: Int) {
    if (!file.fileIsReadOnly) {
      editor.document.remove(start, end - start + 1)
      editor.document.insertString(start, stringToWrite, null)
      grabFocus
    }
  }

  def findNext(f: String, trySelection: Boolean, caseSensitive: Boolean) = editor.findNext(f, trySelection, caseSensitive)

  def findPrevious(f: String, trySelection: Boolean, caseSensitive: Boolean) = editor.findPrevious(f, trySelection, caseSensitive)

  def checkSpelling = editor.checkSpelling(file.dictionary)

  val saveAction = new Action("Save") {
    enabled = false
    title = "Save"

    def apply() {
      saveOrSaveAs("")
    }
  }
  def updateEnabledness = new PropertyChangeListener() {
    def propertyChange(propertyChangeEvent: PropertyChangeEvent) {
      saveAction.enabled = editor.fileIsDirty
    }
  }
  editor.fakeAction.peer.addPropertyChangeListener(updateEnabledness)
}
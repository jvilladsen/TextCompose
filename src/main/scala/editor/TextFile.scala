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
import java.awt.Font
import java.util.Date
import java.awt.FileDialog
import writesetter.{ core, storage }
import writesetter.core.PreviewType

class TextFile {

  var fileIsReadOnly = false
  var fileLatestTimeStamp = 0L
  var fileLatestUpdated = "" // fileLatestTimeStamp converted to text
  var fileName = ""
  var encoding = ""
  var dictionary = ""
  var fileDirectory = ""
  var fullFileName = ""
  var untitledIndex = 1

  def getFileKey: String = {
    if (fullFileName != "") {
      fullFileName
    } else {
      core.Environment.GetConfigFilePath("untitled " + untitledIndex.toString + ".wr")
    }
  }

  private val labelFont = storage.GUIFonts.getStandardFont(14)
  private val fileStatusLabel = new Label {
    foreground = Colors.statusLabel
    border = Swing.EmptyBorder(0, 0, 1, 20) // top, left, bottom, right
    font = labelFont
  }
  private val fullFileNameLabel = new Label {
    foreground = Colors.fileInfo
    border = Swing.EmptyBorder(0, 0, 1, 20) // top, left, bottom, right
    font = labelFont
  }
  private val fileEncodingLabel = new Label {
    foreground = Colors.fileInfo
    border = Swing.EmptyBorder(0, 0, 1, 20) // top, left, bottom, right
    font = labelFont
  }
  private val fileLatestUpdatedLabel = new Label {
    foreground = Colors.fileInfo
    border = Swing.EmptyBorder(0, 0, 1, 0) // top, left, bottom, right
    font = labelFont
  }

  val editorPropertiesPane = new BoxPanel(Orientation.Horizontal) {
    contents += fileStatusLabel
    contents += fullFileNameLabel
    contents += fileEncodingLabel
    contents += fileLatestUpdatedLabel
    background = Colors.editorBackground
    border = Swing.EmptyBorder(2, 10, 0, 0) // top, left, bottom, right
    minimumSize = new Dimension(12, 14)
  }

  def updateColors() {
    fileStatusLabel.foreground = Colors.statusLabel
    fullFileNameLabel.foreground = Colors.fileInfo
    fileEncodingLabel.foreground = Colors.fileInfo
    fileLatestUpdatedLabel.foreground = Colors.fileInfo
    editorPropertiesPane.background = Colors.editorBackground
  }

  private def updatePropertiesPane() {
    fileStatusLabel.text =
      if (fullFileName == "") "Not Saved" else { if (fileIsReadOnly) "Read-Only" else "Writable" }
    fullFileNameLabel.text = fullFileName
    fileEncodingLabel.text = encoding
  }

  def updateTimeStamp(t: Long) {
    fileLatestTimeStamp = t
    fileLatestUpdated = (new Date(t)).toString
    fileLatestUpdatedLabel.text = fileLatestUpdated
  }

  def updateTimeStampAfterSave() {
    updateTimeStamp(storage.FileMethods.GetTimeStamp(fullFileName))
  }

  def newText(number: Int) {
    fileName = "untitled " + number.toString
    untitledIndex = number
    fileDirectory = ""
    fullFileName = ""
    encoding = storage.Configurations.GetCharacterEncoding
    dictionary = storage.Configurations.GetDefaultDictionary
    updatePropertiesPane()
  }

  private def ensureExtensionOnFullFileName() {
    val pathList = fullFileName.split(core.Environment.FileSeparator)
    val pathLength = pathList.length
    var localFileName = if (pathLength > 0) { pathList(pathLength - 1) } else { "" }
    val dotList = localFileName.split('.')
    val dotLength = dotList.length
    if (dotLength == 1) fullFileName += ".wr"
  }

  def updateFromFullName() {
    var FileHandle = new java.io.File(fullFileName)
    fileIsReadOnly = !FileHandle.canWrite
    updateTimeStamp(FileHandle.lastModified)

    val pathList = fullFileName.split(core.Environment.FileSeparator)
    val len = pathList.length
    if (len > 0) { fileName = pathList(len - 1) }

    fileDirectory = ""
    if (len > 1) { fileDirectory = pathList(0) }
    var index = 0
    while (index < len - 1) {
      if (index > 0) fileDirectory += core.Environment.FileSeparator
      fileDirectory += pathList(index)
      index += 1
    }
    updatePropertiesPane()
  }

  def chooseFile(forcedEncoding: String) {
    var openFileChooser = new java.awt.FileDialog(Application.top.peer, "Open File", FileDialog.LOAD)
    openFileChooser.setDirectory(storage.Configurations.GetLatestDirectory("OpenFile"))
    openFileChooser.setVisible(true)

    if (openFileChooser.getFile != null) {
      fullFileName = openFileChooser.getDirectory + openFileChooser.getFile
      encoding = storage.SourcesMetaData.getEncoding(fullFileName, forcedEncoding)
      storage.Configurations.updateLatestDirectory(openFileChooser.getDirectory, "OpenFile")
      updateFromFullName()
    } else {
      throw new Exception("User escaped out of file chooser")
    }
    openFileChooser.dispose
  }

  def openNamedFile(name: String) {
    fullFileName = name
    encoding = storage.SourcesMetaData.getEncoding(fullFileName, "")
    dictionary = storage.SourcesMetaData.getDictionary(fullFileName)
    updateFromFullName()
  }

  def updateDictionary(d: String) {
    dictionary = d
    storage.SourcesMetaData.updateFileData(fullFileName, fileName, encoding, dictionary)
  }

  def checkFileStamp: Boolean = {
    var completed = true
    if (!fileIsReadOnly) {

      if (storage.FileMethods.IsFile(fullFileName) &&
        fileLatestTimeStamp < storage.FileMethods.GetTimeStamp(fullFileName)) {
        val message = "This file has been modified from elsewhere. " +
          "If you continue, you will overwrite a newer version with an older version!"
        completed = DialogBox.warning(message)
      }
    }
    completed
  }

  def chooseFileForSaving(forcedEncoding: String, moveOrRename: Boolean): Boolean = {
    var completed = false
    val openingDirectory =
      if (fullFileName == "") storage.Configurations.GetLatestDirectory("SaveFile") else fileDirectory
    val dialogTitle = if (moveOrRename) "Move or Rename File" else "Save File"
    val saveFileChooser = new java.awt.FileDialog(Application.top.peer, dialogTitle, FileDialog.SAVE)
    saveFileChooser.setDirectory(openingDirectory)
    if (fullFileName != "") saveFileChooser.setFile(fileName)
    saveFileChooser.setVisible(true)

    if (saveFileChooser.getFile != null) {
      fullFileName = saveFileChooser.getDirectory + saveFileChooser.getFile
      encoding = storage.SourcesMetaData.getEncoding(fullFileName, forcedEncoding)
      ensureExtensionOnFullFileName()
      storage.Configurations.updateLatestDirectory(saveFileChooser.getDirectory, "SaveFile")
      updatePropertiesPane()
      completed = true
    }
    saveFileChooser.dispose
    completed
  }

  def showInFinder() {
    if (fullFileName != "") {
      if (!storage.FileMethods.IsFile(fullFileName)) {
        throw new Exception("There is no file '" + fullFileName + "'.")
      }
      val osName = core.Environment.OperatingSystemName
      val command =
        if (core.Environment.isMacOSX) {
          Array[String]("open", "-R", fullFileName)
        } else if (core.Environment.isLinux) {
          Array[String]("xdg-open", fileDirectory)
        } else if (core.Environment.isWindows) {
          Array[String]("explorer", fileDirectory)
        } else {
          throw new Exception("Not prepared to show in file system on '" + osName + "'.")
        }
      val runTime = Runtime.getRuntime()
      val process = runTime.exec(command)
    }
  }

  private def getPDFFileName: (String, String) = {
    val args = new core.Arguments(
      true, // internal
      getFileKey,
      fullFileName == "", // temporaryLocation
      PreviewType.No)
    (args.PDFFileName, args.PDFDefaultTitle)
  }

  def viewPDF() {
    val fileName = getPDFFileName._1
    if (storage.FileMethods.IsFile(fileName)) {
      DesktopInteraction.OpenPDF(fileName, false, "", 0) // fileTitle only used for preview
    } else {
      DialogBox.info("You must build the document before you can view it.")
    }
  }

  def unRegisterInclusion() {
    if (storage.Configurations.IsKnownInclusion(fullFileName)) {
      var confirmed = DialogBox.warning("Make the file '" + fileName + "' unavailable for inclusion? It will not be deleted, just removed from the list of inclusions.")
      if (confirmed) {
        storage.Configurations.unRegisterInclusion(fullFileName)
      }
    } else {
      DialogBox.info("This file (" + fileName + ") is not in the list of inclusions.")
    }
  }

  def unRegisterTemplate() {
    if (storage.Configurations.IsKnownTemplate(fullFileName)) {
      var confirmed = DialogBox.warning("Make the file '" + fileName + "' unavailable as a template? It will not be deleted, just removed from the list of templates.")
      if (confirmed) {
        storage.Configurations.unRegisterTemplate(fullFileName)
      }
    } else {
      DialogBox.info("This file (" + fileName + ") is not in the list of templates.")
    }
  }
}
/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import swing._
import java.awt.Font
import java.util.Date
import java.awt.FileDialog
import textcompose.{ core, storage }
import textcompose.core.PreviewType

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
      core.Environment.getConfigFilePath("untitled " + untitledIndex.toString + ".tc")
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
      if (fullFileName == "") "Not Saved" else if (fileIsReadOnly) "Read-Only" else "Writable"
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
    val fileHandle = new java.io.File(fullFileName)
    val localName = fileHandle.getName
    val dotList = localName.split('.')
    val dotLength = dotList.length
    if (dotLength == 1) fullFileName += ".tc"
  }

  def updateFromFullName() {
    val fileHandle = new java.io.File(fullFileName)
    fileIsReadOnly = !fileHandle.canWrite
    updateTimeStamp(fileHandle.lastModified)
    fileName = fileHandle.getName
    fileDirectory = fileHandle.getParent + core.Environment.fileSeparator
    updatePropertiesPane()
  }

  def chooseFile(forcedEncoding: String) {
    val openFileChooser = new java.awt.FileDialog(Application.top.peer, "Open File", FileDialog.LOAD)
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

      val difference = storage.FileMethods.GetTimeStamp(fullFileName) - fileLatestTimeStamp
      if (difference > 0) {
        val message = "This file has been modified from elsewhere. " +
          "If you continue, you will overwrite a newer version with an older version! " +
          "It is newer by " + difference.toString + "ms."
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
      val osName = core.Environment.operatingSystemName
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

  def unregisterExtension() {
    if (storage.Configurations.isKnownExtensionFile(fullFileName)) {
      var confirmed = DialogBox.warning("Make the file '" + fileName +
        "' unavailable for extension?\nIt will not be deleted, just removed from the list of extensions.")
      if (confirmed) {
        storage.Configurations.unregisterExtension(fullFileName)
      }
    } else {
      DialogBox.info("This file (" + fileName + ") is not in the list of extensions.")
    }
  }

  def unRegisterTemplate() {
    if (storage.Configurations.IsKnownTemplate(fullFileName)) {
      var confirmed = DialogBox.warning("Make the file '" + fileName +
        "' unavailable as a template?\nIt will not be deleted, just removed from the list of templates.")
      if (confirmed) {
        storage.Configurations.unRegisterTemplate(fullFileName)
      }
    } else {
      DialogBox.info("This file (" + fileName + ") is not in the list of templates.")
    }
  }
}
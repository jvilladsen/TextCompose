/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import swing.Component
import textcompose.{ core, editor }

object Configurations extends StoredArrayOfStringLists("Configuration.txt") {

  /*
   * Values in first field:
   * - FontLocation
   * - Extension
   * - TabSize
   * - SaveBeforeCompile
   * - WriteErrorMessagesToDocument
   * - ViewAfterCompile
   * - LatestDirectory
   * - CharacterEncoding
   * - EditorFontName
   * - Theme
   * - DefaultDictionary
   * - PreviewZoomPercentage
   * - ResourcesVersion (internal)
   */

  /** Version number of resources. Increase it by one if there are
    * changes to resources that are copied out to the file system.
    */
  private val currentResourcesVersion = 2

  private var extensionToFileName = new HashMap[String, String]
  private var templateToFileName = new HashMap[String, String]
  private var latestDirectory = new HashMap[String, String]
  private var tabSize = 2
  private var saveBeforeCompile = true
  private var writeErrorMessagesToDocument = true
  private var viewAfterCompile = 0 // 0: no, 1: if no errors, 2: yes
  private var characterEncoding = "UTF-8"
  private var editorFontName = GUIFonts.getStandardFontName
  private var guiTheme = 0
  private var defaultDictionary = "English UK"
  private var previewZoomPercentage = 170
  private var storedResourcesVersion = 0
  private var isHigherResourcesVersion = false

  private var errorsDuringInitialization = new ArrayBuffer[String]

  override def getKeyLength(configuration: List[String]): Int = {
    if (configuration(0) == "FontLocation"
      || configuration(0) == "Extension"
      || configuration(0) == "Template"
      || configuration(0) == "LatestDirectory") {
      2
    } else {
      1
    }
  }

  private def extractFromDataSet() {
    var toBeRemoved = new ArrayBuffer[List[String]]
    for (configuration <- dataSet) {
      val length = configuration.length
      configuration(0) match {
        case "FontLocation" => {
          if (length == 2) core.FontFileRegister.addDirectory(configuration(1))
        }
        case "Extension" => {
          if (length == 3) {
            val extensionName = configuration(1)
            val fullFileName = configuration(2)
            if (FileMethods.IsFile(fullFileName)) {
              extensionToFileName += extensionName -> fullFileName
            } else {
              toBeRemoved += configuration
              val message = "Previously registered extension '" + extensionName + "' has been renamed/removed. " +
                "\nIt was placed in '" + fullFileName + "'."
              errorsDuringInitialization += message
            }
          }
        }
        case "Template" => {
          if (length == 3) {
            val templateName = configuration(1)
            val fullFileName = configuration(2)
            if (FileMethods.IsFile(fullFileName)) {
              templateToFileName += templateName -> fullFileName
            } else {
              toBeRemoved += configuration
              val message = "Previously registered template '" + templateName + "' has been renamed/removed. " +
                "\nIt was placed in '" + fullFileName + "'."
              errorsDuringInitialization += message
            }
          }
        }
        case "TabSize" => {
          if (length == 2) tabSize = configuration(1).toInt
        }
        case "SaveBeforeCompile" => {
          if (length == 2) saveBeforeCompile = configuration(1).toBoolean
        }
        case "WriteErrorMessagesToDocument" => {
          if (length == 2) writeErrorMessagesToDocument = configuration(1).toBoolean
        }
        case "ViewAfterCompile" => {
          if (length == 2) viewAfterCompile = configuration(1).toInt
        }
        case "LatestDirectory" => {
          if (length == 3) latestDirectory += configuration(1) -> configuration(2)
        }
        case "CharacterEncoding" => {
          if (length == 2) characterEncoding = configuration(1)
        }
        case "EditorFontName" => {
          if (length == 2) editorFontName = configuration(1)
        }
        case "GUITheme" => {
          if (length == 2) guiTheme = configuration(1).toInt
        }
        case "DefaultDictionary" => {
          if (length == 2) defaultDictionary = configuration(1)
        }
        case "PreviewZoomPercentage" => {
          if (length == 2) previewZoomPercentage = configuration(1).toInt
        }
        case "ResourcesVersion" => {
          if (length == 2) storedResourcesVersion = configuration(1).toInt
        }
      }
    }
    for (configuration <- toBeRemoved) {
      remove(configuration)
    }
  }

  private def addMissingEntries() {

    def updateFontLocation(dir: String) {
      if (FileMethods.IsDirectory(dir)) addIfMissing(List("FontLocation", dir))
    }

    if (core.Environment.isMacOSX) {
      updateFontLocation("/Library/Fonts")
      updateFontLocation(core.Environment.getUserHome + "/Library/Fonts")
    } else if (core.Environment.isLinux) {
      updateFontLocation("/usr/share/fonts")
      updateFontLocation("/usr/local/share/fonts")
      updateFontLocation(core.Environment.getUserHome + "/.fonts")
    } else if (core.Environment.isWindows) {
      updateFontLocation("C:\\Windows\\Fonts")
    }

    addIfMissing(List("TabSize", "2"))
    addIfMissing(List("SaveBeforeCompile", "true"))
    addIfMissing(List("WriteErrorMessagesToDocument", "true"))
    addIfMissing(List("ViewAfterCompile", "2"))
    addIfMissing(List("CharacterEncoding", "UTF-8"))
    addIfMissing(List("EditorFontName", GUIFonts.getStandardFontName))
    addIfMissing(List("GUITheme", "1"))
    addIfMissing(List("DefaultDictionary", "English UK"))
    addIfMissing(List("PreviewZoomPercentage", "170"))
    addIfMissing(List("ResourcesVersion", "0"))
    saveToFile()
  }

  private def compareResourcesVersions() {
    isHigherResourcesVersion = currentResourcesVersion > storedResourcesVersion
    if (isHigherResourcesVersion) {
      update(List("ResourcesVersion", currentResourcesVersion.toString))
      saveToFile()
      storedResourcesVersion = currentResourcesVersion
    }
  }

  def initialize() {
    if (!initialized) {
      core.FontFileRegister.addBuildInFonts()
      loadFromFile()
      addMissingEntries()
      extractFromDataSet()
      initialized = true
      if (!errorsDuringInitialization.isEmpty) {
        saveToFile()
      }
      compareResourcesVersions()
    }
  }

  def showErrorsDuringInitialization() {
    for (message <- errorsDuringInitialization) {
      editor.DialogBox.error(message)
    }
  }

  def registerNewExtension(extensionName: String, fileName: String) {
    extensionToFileName += extensionName -> fileName
    update(List("Extension", extensionName, fileName))
    saveToFile()
    core.Parsers.updateInclude()
  }

  def isKnownExtensionFile(fileName: String): Boolean = {
    var found = false
    for (i <- extensionToFileName) {
      if (i._2 == fileName) { found = true }
    }
    found
  }

  def unregisterExtension(fileName: String) {
    val extensions = new ArrayBuffer[String]
    for (i <- extensionToFileName) {
      if (i._2 == fileName) { extensions += i._1 }
    }
    for (i <- extensions) {
      extensionToFileName.remove(i)
      remove(List("Extension", i, fileName))
    }
    saveToFile()
    core.Parsers.updateInclude()
  }

  def registerNewTemplate(templateName: String, fileName: String) {
    templateToFileName += templateName -> fileName
    update(List("Template", templateName, fileName))
    saveToFile()
  }

  def IsKnownTemplate(fileName: String): Boolean = {
    var found = false
    for (i <- templateToFileName) {
      if (i._2 == fileName) { found = true }
    }
    found
  }

  def unRegisterTemplate(fileName: String) {
    var templates = new ArrayBuffer[String]
    for (i <- templateToFileName) {
      if (i._2 == fileName) { templates += i._1 }
    }
    for (i <- templates) {
      templateToFileName.remove(i)
      remove(List("Template", i, fileName))
    }
    saveToFile()
  }

  def updateLatestDirectory(directory: String, context: String) {
    latestDirectory += context -> directory
    update(List("LatestDirectory", context, directory))
    saveToFile()
  }

  def setDefaults(tab: Int, save: Boolean, writeErrors: Boolean, preview: Int, encoding: String, editorFont: String,
    dictionary: String, previewZoom: Int) {

    val doUpdateFont = editorFontName != editorFont
    tabSize = tab
    saveBeforeCompile = save
    writeErrorMessagesToDocument = writeErrors
    viewAfterCompile = preview
    characterEncoding = encoding
    editorFontName = editorFont
    defaultDictionary = dictionary
    previewZoomPercentage = previewZoom
    update(List("TabSize", tab.toString))
    update(List("SaveBeforeCompile", save.toString))
    update(List("WriteErrorMessagesToDocument", writeErrors.toString))
    update(List("ViewAfterCompile", preview.toString))
    update(List("CharacterEncoding", encoding))
    update(List("EditorFontName", editorFont))
    update(List("DefaultDictionary", dictionary))
    update(List("PreviewZoomPercentage", previewZoom.toString))
    saveToFile()
    if (doUpdateFont) editor.Application.workspaceTabs.updateFontInEditors
  }

  def setTheme(theme: Int) {
    guiTheme = theme
    update(List("GUITheme", theme.toString))
    saveToFile()
  }

  def isKnownExtensionName(extension: String): Boolean = extensionToFileName.isDefinedAt(extension)

  def getExtensionFileName(extension: String): String = extensionToFileName(extension)

  def getListOfExtensions: List[String] = extensionToFileName.keys.toList.sortWith((a, b) => a < b)

  def isKnownTemplateName(template: String): Boolean = templateToFileName.isDefinedAt(template)

  def GetTemplateFileName(template: String): String = templateToFileName(template)

  def GetListOfTemplates: List[String] = templateToFileName.keys.toList.sortWith((a, b) => a < b)

  def GetTabSize: Int = tabSize
  def GetSaveBeforeCompile: Boolean = saveBeforeCompile
  def GetWriteErrorMessagesToDocument: Boolean = writeErrorMessagesToDocument
  def getViewAfterCompile: Int = viewAfterCompile
  def GetCharacterEncoding = characterEncoding
  def GetEditorFontName = editorFontName
  def GetDefaultDictionary = defaultDictionary
  def getPreviewZoomPercentage = previewZoomPercentage
  def getTheme = guiTheme
  def doUpdateResourcesNow = isHigherResourcesVersion

  def GetLatestDirectory(context: String): String = {
    if (latestDirectory.isDefinedAt(context)) {
      latestDirectory(context)
    } else {
      core.Environment.getUserHome
    }
  }
}
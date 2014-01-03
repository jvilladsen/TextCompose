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

package writesetter.storage

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import swing.Component
import writesetter.{ core, editor }

object Configurations extends StoredArrayOfStringLists("Configuration.txt") {

  /*
	 * Values in first field:
	 * - FontLocation
	 * - Inclusion
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
	 */

  private var inclusionToFileName = new HashMap[String, String]
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

  private var errorsDuringInitialization = new ArrayBuffer[String]

  override def getKeyLength(configuration: List[String]): Int = {
    if (configuration(0) == "FontLocation"
      || configuration(0) == "Inclusion"
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
      if (configuration(0) == "FontLocation" && configuration.length == 2) {
        core.FontFileRegister.addDirectory(configuration(1))
      } else if (configuration(0) == "Inclusion" && configuration.length == 3) {
        val inclusionName = configuration(1)
        val fullFileName = configuration(2)
        if (FileMethods.IsFile(fullFileName)) {
          inclusionToFileName += inclusionName -> fullFileName
        } else {
          toBeRemoved += configuration
          val message = "Previously registered inclusion '" + inclusionName + "' has been renamed/removed. " +
            "\nIt was placed in '" + fullFileName + "'."
          errorsDuringInitialization += message
        }
      } else if (configuration(0) == "Template" && configuration.length == 3) {
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
      } else if (configuration(0) == "TabSize" && configuration.length == 2) {
        tabSize = configuration(1).toInt
      } else if (configuration(0) == "SaveBeforeCompile" && configuration.length == 2) {
        saveBeforeCompile = configuration(1).toBoolean
      } else if (configuration(0) == "WriteErrorMessagesToDocument" && configuration.length == 2) {
        writeErrorMessagesToDocument = configuration(1).toBoolean
      } else if (configuration(0) == "ViewAfterCompile" && configuration.length == 2) {
        viewAfterCompile = configuration(1).toInt
      } else if (configuration(0) == "LatestDirectory" && configuration.length == 3) {
        latestDirectory += configuration(1) -> configuration(2)
      } else if (configuration(0) == "CharacterEncoding" && configuration.length == 2) {
        characterEncoding = configuration(1)
      } else if (configuration(0) == "EditorFontName" && configuration.length == 2) {
        editorFontName = configuration(1)
      } else if (configuration(0) == "GUITheme" && configuration.length == 2) {
        guiTheme = configuration(1).toInt
      } else if (configuration(0) == "DefaultDictionary" && configuration.length == 2) {
        defaultDictionary = configuration(1)
      } else if (configuration(0) == "PreviewZoomPercentage" && configuration.length == 2) {
        previewZoomPercentage = configuration(1).toInt
      }
    }
    for (configuration <- toBeRemoved) {
      remove(configuration)
    }
  }

  def StoreDefaults {
    
    def updateFontLocation(dir: String) {
      if (FileMethods.IsDirectory(dir)) update(List("FontLocation", dir))
    }
    
    if (core.Environment.isMacOSX) {
      updateFontLocation("/Library/Fonts")
      updateFontLocation(core.Environment.CurrentUserHome + "/Library/Fonts")
    } else if (core.Environment.isLinux) {
      updateFontLocation("/usr/share/fonts")
      updateFontLocation("/usr/local/share/fonts")
      updateFontLocation(core.Environment.CurrentUserHome + "/.fonts")
    } else if (core.Environment.isWindows) {
      updateFontLocation("C:\\Windows\\Fonts")
    }

    update(List("TabSize", "2"))
    update(List("SaveBeforeCompile", "true"))
    update(List("WriteErrorMessagesToDocument", "true"))
    update(List("ViewAfterCompile", "2"))
    update(List("CharacterEncoding", "UTF-8"))
    update(List("EditorFontName", GUIFonts.getStandardFontName))
    update(List("GUITheme", "0"))
    update(List("DefaultDictionary", "English UK"))
    update(List("PreviewZoomPercentage", "170"))
    store()
  }

  def initialize {
    if (!initialized) {
      core.FontFileRegister.addBuildInFonts
      if (!fileExists) { StoreDefaults }
      load()
      extractFromDataSet()
      initialized = true
      if (!errorsDuringInitialization.isEmpty) {
        store() // Store if there were any errors.
      }
    }
  }

  def ShowErrorsDuringInitialization {
    for (message <- errorsDuringInitialization) {
      editor.DialogBox.error(message)
    }
  }

  def registerNewInclusion(inclusionName: String, fileName: String) {
    inclusionToFileName += inclusionName -> fileName
    update(List("Inclusion", inclusionName, fileName))
    store()
  }

  def IsKnownInclusion(fileName: String): Boolean = {
    var found = false
    for (i <- inclusionToFileName) {
      if (i._2 == fileName) { found = true }
    }
    found
  }

  def unRegisterInclusion(fileName: String) {
    var inclusions = new ArrayBuffer[String]
    for (i <- inclusionToFileName) {
      if (i._2 == fileName) { inclusions += i._1 }
    }
    for (i <- inclusions) {
      inclusionToFileName.remove(i)
      remove(List("Inclusion", i, fileName))
    }
    store()
  }

  def registerNewTemplate(templateName: String, fileName: String) {
    templateToFileName += templateName -> fileName
    update(List("Template", templateName, fileName))
    store()
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
    store()
  }

  // setters

  def updateLatestDirectory(directory: String, context: String) {
    latestDirectory += context -> directory
    update(List("LatestDirectory", context, directory))
    store()
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
    store()
    if (doUpdateFont) editor.Application.workspaceTabs.updateFontInEditors
  }

  def setTheme(theme: Int) {
    guiTheme = theme
    update(List("GUITheme", theme.toString))
    store()
  }

  // getters

  def IsKnownInclusionName(inclusion: String): Boolean = inclusionToFileName.isDefinedAt(inclusion)

  def GetInclusionFileName(inclusion: String): String = inclusionToFileName(inclusion)

  def GetListOfInclusions: List[String] = inclusionToFileName.keys.toList.sortWith((a, b) => a < b)

  def IsKnownTemplateName(template: String): Boolean = templateToFileName.isDefinedAt(template)

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

  def GetLatestDirectory(context: String): String = {
    if (latestDirectory.isDefinedAt(context)) {
      latestDirectory(context)
    } else {
      core.Environment.CurrentUserHome
    }
  }
}
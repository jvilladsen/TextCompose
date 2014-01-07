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

import scala.io.Source
import writesetter.{editor, storage}

object FileRegistration {

  var fullFileName = ""
  var fileType = ""
  var tagFound = false
  var parameters = 0
  var name = ""

  private def Parse {
    tagFound = false
    var src = Source fromFile (fullFileName)
    for (line <- src.getLines) {
      var WellFormedLine = true
      var ElmStack = new SourceElementStack(false)
      try {
        ElmStack.ParseLine(line)
      } catch {
        case e: Exception => WellFormedLine = false
      }
      if (WellFormedLine) {
        for (SElm <- ElmStack.LineElements) {
          if (!tagFound && SElm.IsTag && SElm.TagName == fileType) {
            tagFound = true
            parameters = SElm.NumberOfParameters
            if (parameters > 0) {
              name = SElm.Parameters(0)
            }
          }
        }
      }
    }
  }

  private def Register {
    if (tagFound && parameters > 0) {
      var continue = true
      if (fileType == "extension") {
        if (storage.Configurations.isKnownExtensionName(name)
          && storage.Configurations.getExtensionFileName(name) != fullFileName) {
          val message = "There is already an extension named '" + name + "', registered from\n'" +
            storage.Configurations.getExtensionFileName(name) + "'.\n\n" +
            "This extension name will now refer to the file '\n" +
            fullFileName
          continue = editor.DialogBox.warning(message)
        }
        if (continue) {
          storage.Configurations.registerNewExtension(name, fullFileName)
        }
      } else {
        if (name == "none") {
          val message = "Of all names, the name 'none' is not permitted for a template."
          editor.DialogBox.error(message)
          continue = false
        } else if (storage.Configurations.IsKnownTemplateName(name)
          && storage.Configurations.GetTemplateFileName(name) != fullFileName) {
          val message = "There is already an template named '" + name + "', registered from\n'" +
            storage.Configurations.GetTemplateFileName(name) + "'.\n\n" +
            "This template name will now refer to the file '\n" +
            fullFileName
          continue = editor.DialogBox.warning(message)
        }
        if (continue) {
          storage.Configurations.registerNewTemplate(name, fullFileName)
        }
      }
    } else if (!tagFound) {
      val message = if (fileType == "extension") {
        "This file has no 'extension' tag. Extension files must have such a tag, specifying the name of the extension."
      } else {
        "This file has no 'template' tag. Templates must have such a tag, specifying the name of the template."
      }
      editor.DialogBox.error(message)
    } else {
      val message = if (fileType == "extension") {
        "The extension tag should have one parameter with the name of the extension."
      } else {
        "The template tag should have a parameter with the name of the template."
      }
      editor.DialogBox.error(message)
    }
  }

  def NewFile(n: String, t: String) {
    if (n == "") {
      editor.DialogBox.error("The file must be saved before it can be added as " + t + ".")
    } else {
      fullFileName = n
      fileType = t
      if (fileType == "extension" || fileType == "template") {
        Parse
        Register
      } else {
        editor.DialogBox.systemError("File type for registration must be 'extension' or 'template'. Got '" + fileType + "'.")
      }
    }
  }
}
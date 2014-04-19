/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.io.Source
import textcompose.{editor, storage}

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
        } else if (storage.Configurations.isKnownTemplateName(name)
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
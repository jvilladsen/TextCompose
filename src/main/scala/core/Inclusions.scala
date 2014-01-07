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

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import writesetter.{ editor, storage }

class Extensions {

  var extensionNames = new HashSet[String] // to ensure that we don't try to include the same twice

  var TagDefinitions = new HashMap[String, TagDefinition] // tag name -> tag definition

  var errorMessages = new ArrayBuffer[(String, ProcessingUnit)]

  var mainTags = new ArrayBuffer[String]

  TagRegister.addBuiltInTags

  def cleanUpAfterReadingExtension {
    errorMessages.clear
    mainTags.clear
  }

  def UserDefinedTag(tagName: String): Boolean = TagDefinitions.contains(tagName)

  private def parseExtension(
    extensionName: String,
    fullFileName: String,
    encoding: String,
    processingUnit: ProcessingUnit) {

    var Source = new SourceFile(fullFileName, encoding, processingUnit, false)
    var lineContent = ""
    var lineNumber = 0

    var insideTagDefinition = false
    var definitionType = ""
    var currentTagName = ""
    var WellFormedLine = true

    while (Source.readLine) {
      lineNumber += 1
      try {
        lineContent = processingUnit.getLine
      } catch {
        case e: Exception => editor.DialogBox.stackTrace("Error reading line " + lineNumber.toString + " from " + fullFileName + ". Wrong encoding?", e)
      }

      if (insideTagDefinition) {
        val timmedLine = lineContent.trim
        if (timmedLine == "</def>" || timmedLine == "</sub>" || timmedLine == "</main>") {
          insideTagDefinition = false
          val subTrimmed = timmedLine.substring(2, timmedLine.length - 1)
          if (definitionType != subTrimmed) {
            errorMessages.append(("Error in the definition of the tag '" + currentTagName + "' in the extension '" +
              extensionName + "': Finish 'def' with '/def', 'sub' with '/sub' and 'main' with '/main'.", processingUnit))
          }
        } else {
          try {
            TagDefinitions(currentTagName).ParseLine(lineContent)
          } catch {
            case pe: ParseError => errorMessages.append((pe.errorMessage, processingUnit))
            case te: TagError   => errorMessages.append((te.errorMessage, processingUnit))
          }
        }
      } else {
        WellFormedLine = true
        var ses = new SourceElementStack(false)
        try {
          ses.ParseLine(lineContent)
        } catch {
          case e: ParseError =>
            WellFormedLine = false
            errorMessages.append((e.errorMessage, processingUnit))
        }
        if (WellFormedLine && !ses.LineElements.isEmpty && ses.LineElements(0).IsTag) {
          val tagName = ses.LineElements(0).TagName
          if (tagName == "def" || tagName == "sub" || tagName == "main") {
            definitionType = tagName
            val isMainTag = tagName == "main"

            var td = new TagDefinition(ses.LineElements(0), extensionName, lineNumber + 1, isMainTag)
            // FIXME (+1 above) : change this when it becomes possible to define a tag on one single line.
            currentTagName = td.tagName

            if (isMainTag) mainTags.append(currentTagName)

            if (TagDefinitions.contains(currentTagName)) {
              errorMessages.append(("The tag '" + currentTagName + "' has already been defined.", processingUnit))
            }
            TagDefinitions += currentTagName -> td
            if (definitionType == "def") {
              // Neither 'sub' nor 'main' definitions should appear in the tag tree.
              // This is actually the whole point with 'sub'. The point with 'main' definitions
              // is that they get directly into the stream, just by extension.
              LatestExtensions.addTag(extensionName, currentTagName, td)
            }
            TagRegister.AddNewTag(currentTagName)
            insideTagDefinition = true
          } else if (tagName == "include") {
            val e = ses.LineElements(0)
            if (e.NumberOfParameters == 1) {
              addNewExtension(e.Parameters(0), processingUnit)
            } else {
              errorMessages.append(("The 'include' tag should have a parameter.", processingUnit))
            }
          } else if (tagName != "extension") {
            errorMessages.append(("Unexpected tag '" + tagName + "' in the extension '" + extensionName +
              "'. Extensions are meant for specifying new tags using the tag 'def' (or 'sub', 'main'). " +
              "It is also possible to use the 'include' tag in extensions.", processingUnit))
          }
        }
      } // if insideTagDefinition / else
    } // end while
    if (insideTagDefinition) errorMessages.append(("Error in '" + extensionName + "': Incomplete tag definition at end of file.", processingUnit))
  }

  def addNewExtension(extensionName: String, processingUnit: ProcessingUnit) {

    if (!extensionNames.contains(extensionName)) {
      var fullFileName = ""
      var encoding = ""
      if (storage.Configurations.isKnownExtensionName(extensionName)) {
        fullFileName = storage.Configurations.getExtensionFileName(extensionName)
        encoding = storage.SourcesMetaData.getEncoding(fullFileName, "")
      } else {
        throw new TagError("The 'include' tag refers to an extension '" + extensionName +
          "', that has not been registered. To register a file as an extension, simply open the " +
          "file and choose the action 'Add Extension' in the 'Extensions' menu.")
      }

      LatestExtensions.addExtension(extensionName)

      extensionNames += extensionName
      parseExtension(extensionName, fullFileName, encoding, processingUnit)
    }
  }
}
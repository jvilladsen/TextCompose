/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import textcompose.{ editor, storage }

class Extensions {

  val extensionNames = new HashSet[String] // to ensure that we don't try to include the same twice

  val TagDefinitions = new HashMap[String, TagDefinition] // tag name -> tag definition

  val errorMessages = new ArrayBuffer[(String, ProcessingUnit)]

  val mainTags = new ArrayBuffer[String]

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

            var td = new TagDefinition(ses.LineElements(0), extensionName, lineNumber + 1, isMainTag) // FIXME line number
            currentTagName = td.tagName

            if (isMainTag) mainTags.append(currentTagName)

            if (TagDefinitions.contains(currentTagName)) {
              errorMessages.append(("The tag '" + currentTagName + "' has already been defined.", processingUnit))
            }
            TagDefinitions += currentTagName -> td
            if (definitionType == "def") {
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
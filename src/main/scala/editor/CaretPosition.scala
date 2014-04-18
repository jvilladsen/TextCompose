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

package textcompose.editor

import concurrent.ExecutionContext.Implicits.global
import concurrent._
import swing._
import textcompose.core

object CaretPosition {

  var currentEditor: TextFileEditor = null
  var currentPosition = 0
  var currentLength = 0
  var updateTimestamp = 0L
  val millisecondsToWait = 600

  var currentLine = ""
  var positionOnLine = 0

  var wellFormedLine = true
  var isInsideTag = false
  var parseErrorMessage = ""
  var ElmStack = new core.SourceElementStack(true)
  var currentTagName = ""
  var foundTagStartingAt = 0
  var foundTagEndingAt = 0

  var delayIsSpawned = false

  def handleUpdate(fileEditor: TextFileEditor, tagPane: TagPane) {
    currentEditor = fileEditor
    currentPosition = fileEditor.editor.getCursorPosition
    currentLength = fileEditor.editor.document.getLength

    updateTimestamp = java.util.Calendar.getInstance().getTimeInMillis

    if (!delayIsSpawned) {
      delayIsSpawned = true

      future {
        // Wait for the user to take a break in the typing.
        var keepWaiting = true
        while (keepWaiting) {
          Thread.sleep(200)
          val timeSinceLastUpdate = java.util.Calendar.getInstance().getTimeInMillis - updateTimestamp
          if (timeSinceLastUpdate > millisecondsToWait) {
            keepWaiting = false
          }
        }

        try {
	        determineCurrentLine()
	        parseCurrentLine()
	        if (wellFormedLine) {
	          tagPane.updateFromEditor(fileEditor.file.getFileKey,
	            isInsideTag,
	            foundTagStartingAt,
	            foundTagEndingAt,
	            ElmStack.TagAtPosition)
	        } else {
	          tagPane.updateWithParserErrorFromEditor(parseErrorMessage)
	        }
        } catch {
          case e: Exception => DialogBox.stackTrace("Failed in future to maintain tag dialog", e)
        }
        delayIsSpawned = false
      }
    }
  }

  private def determineCurrentLine() {

    def safePosition(p: Int): Int = {
      if (p < 0) {
        return 0
      } else if (p > currentLength) {
        return currentLength
      }
      return p
    }

    def getFirstOrLastBeforeBreak(s: String, f: Boolean): String = {
      val sl = s.split('\n')
      if (sl.length == 0) {
        return ""
      } else if (f) {
        return sl(0)
      } else {
        return sl(sl.length - 1)
      }
    }

    val rightAfterNewline = (currentPosition > 0 && currentEditor.editor.document.getText(currentPosition - 1, 1) == "\n")
    var beforeCurrentPosition = ""
    if (!rightAfterNewline) {
      val start = safePosition(currentPosition - 500)
      var y = currentEditor.editor.document.getText(start, currentPosition - start)
      beforeCurrentPosition = getFirstOrLastBeforeBreak(y, false)
    }

    val stop = safePosition(currentPosition + 500)
    var y = currentEditor.editor.document.getText(currentPosition, stop - currentPosition)
    var afterCurrentPosition = getFirstOrLastBeforeBreak(y, true)
    currentLine = beforeCurrentPosition + afterCurrentPosition
    positionOnLine = beforeCurrentPosition.length
    // FIXME: The above is not correct in general, because of the 500 character limit on both directions from current position.
  }

  private def parseCurrentLine() {
    wellFormedLine = true
    parseErrorMessage = ""
    ElmStack.setPositionForMatching(positionOnLine)

    try {
      ElmStack.ParseLine(currentLine)
    } catch {
      case e: core.ParseError => wellFormedLine = false; parseErrorMessage = e.getMessage
    }

    isInsideTag = ElmStack.TagFoundAtPosition // wellFormedLine && 

    if (isInsideTag) {
      currentTagName = ElmStack.TagAtPosition.TagName
      foundTagStartingAt = currentPosition + ElmStack.TagFoundStartsAt
      foundTagEndingAt = currentPosition + ElmStack.TagFoundEndsAt
      ElmStack.TagFoundAtPosition = false
    } else {
      currentTagName = ""
    }
  }
}
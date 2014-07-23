/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
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

  var delayedHandlerSpawned = false // Should be out here!

  def handleUpdate(fileEditor: TextFileEditor, tagPane: TagPane) {
    currentEditor = fileEditor
    currentPosition = fileEditor.editor.getCursorPosition
    currentLength = fileEditor.editor.document.getLength

    val currentTimeInMillis = java.util.Calendar.getInstance().getTimeInMillis
    def waitingTime = java.util.Calendar.getInstance().getTimeInMillis - currentTimeInMillis

    if (!delayedHandlerSpawned) {
      delayedHandlerSpawned = true

      future {
        while (waitingTime <= millisecondsToWait) Thread.sleep(200)

        try {
          determineCurrentLine()
          parseCurrentLine()
          if (wellFormedLine) {
            tagPane.updateFromEditor(
              fileEditor.file.getFileKey,
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
        delayedHandlerSpawned = false
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
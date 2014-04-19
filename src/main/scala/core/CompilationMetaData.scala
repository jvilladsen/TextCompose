/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.collection.mutable.ArrayBuffer

object CompilationMetaData {

  private var latestSourceFullName = ""
  private var isTemporaryLocation = false
  private var correspondingPDFFullName = ""
  private var pageCount = 0
  private var errorCount = 0
  private var errorMessages: ArrayBuffer[(String, String)] = null
  private var startTimestamp = 0L
  private var endTimestamp = 0L
  private var hasContent = false
  private var complete = false

  private def getTime = java.util.Calendar.getInstance().getTimeInMillis

  def init(arg: Arguments) {
    latestSourceFullName = arg.sourceFullFileName
    isTemporaryLocation = arg.isTemporaryLocation
    correspondingPDFFullName = arg.PDFFileName
    hasContent = false
    pageCount = 0
    errorCount = 0
    errorMessages = new ArrayBuffer[(String, String)]
    complete = false
    startTimestamp = getTime
  }
  def addError(message: String, location: String) {
    errorCount += 1
    errorMessages.append((message, location))
  }
  def stopTimer() { endTimestamp = getTime }
  def setHasContent(pages: Int) {
    hasContent = true
    pageCount = pages
  }

  def end() {
    complete = true
    if (!isTemporaryLocation) {
      textcompose.storage.SourcesMetaData.updateCompilationMetaData(
        latestSourceFullName,
        pageCount,
        errorCount,
        startTimestamp,
        getDuration)
    }
  }

  def getDuration = endTimestamp - startTimestamp
  def getHasContent = hasContent
  def getErrorCount = errorCount
  def getErrorMessages = errorMessages
}
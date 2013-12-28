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
      writesetter.storage.SourcesMetaData.updateCompilationMetaData(
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
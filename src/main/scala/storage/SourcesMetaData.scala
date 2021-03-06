/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

import scala.collection.mutable.Stack
import textcompose.editor

object SourcesMetaData extends StoredArrayOfStringLists("SourcesMetaData.txt") {

  /*
   * Fields:
   * - Full file name (primary key)
   * - File name
   * - Time stamp for creation of source file
   * - Time stamp for latest save
   * - Character encoding for latest save
   * - Dictionary (for spelling) - "language"
   * - Page count
   * - Error count
   * - Built duration
   * - Time of latest build
   * - Number of pages in latest build
   * - Number of errors in latest build
   *
   * Still to come:
   * - Size of PDF (in bytes, kilo bytes, mega bytes,...)
   * - Title, Author, Subject, Keywords
   * - Draft/Final (State/Stage/Status)
   * - Deadline
   * - Next step
   */

  minimumFieldCount = 10

  override def getKeyLength(configuration: List[String]) = 1

  def initialize() {
    if (!initialized) {
      if (fileExists) loadFromFile()
      initialized = true
    }
  }

  def getListOfFileNames: List[List[String]] = {
    val result = new Stack[List[String]]
    for (configuration <- dataSet) {
      val fullFileName = configuration(0)
      val fileName = configuration(1)
      val time = configuration(3) // updated time
      if (FileMethods.IsFile(fullFileName)) {
        result.push(List(fullFileName, fileName, time))
      } else {
        removeFileData(fullFileName)
      }
    }
    result.toList.sortWith((a, b) => a(2).toLong > b(2).toLong)
  }

  def getEncoding(fullFileName: String, forcedEncoding: String): String = {
    val i = getIndexOf(List(fullFileName))
    val storedEncoding = if (i == -1) "" else dataSet(i)(4)

    val continue =
      if (forcedEncoding != ""
        && storedEncoding != ""
        && forcedEncoding != storedEncoding) {

        val message = "This file has previously been saved with encoding " + storedEncoding
        editor.DialogBox.warning(message)
      } else {
        true
      }
    if (forcedEncoding != "" && continue) {
      forcedEncoding
    } else if (storedEncoding != "") {
      storedEncoding
    } else {
      Configurations.GetCharacterEncoding
    }
  }

  def getCreationTime(fullFileName: String): String = {
    val i = getIndexOf(List(fullFileName))
    if (i == -1) "" else dataSet(i)(2)
  }

  def getDictionary(fullFileName: String): String = {
    val i = getIndexOf(List(fullFileName))
    if (i == -1) "" else dataSet(i)(5)
  }

  def updateFileData(fullFileName: String, fileName: String, encoding: String, dictionary: String) {
    val time = FileMethods.GetTimeStamp(fullFileName)
    // FIXME: the first time stamp should be creation time.
    val storedCreationTime = getCreationTime(fullFileName)
    val creationTime = if (storedCreationTime == "") time.toString else storedCreationTime
    // FIXME: check that encoding and dictionary are valid.
    update(List(fullFileName, fileName, creationTime, time.toString, encoding, dictionary))
    saveToFile()
  }

  def updateCompilationMetaData(fullFileName: String, pageCount: Int, errorCount: Int, builtTime: Long, builtDuration: Long) {
    updateFrom(List(fullFileName), 6, List(pageCount.toString, errorCount.toString, builtTime.toString, builtDuration.toString))
    saveToFile()
  }

  def removeFileData(fullFileName: String) {
    remove(List(fullFileName))
    saveToFile()
  }

  def renameFileData(originalFileName: String, newFullFileName: String, newFileName: String) {
    val i = getIndexOf(List(originalFileName))
    if (i == -1) {
      editor.DialogBox.systemError("Unknown file " + originalFileName)
    } else {
      dataSet(i) = newFullFileName :: newFileName :: dataSet(i).tail.tail
    }
    saveToFile()
  }
}
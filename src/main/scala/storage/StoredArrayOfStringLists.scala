/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.storage

import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import textcompose.{ core, editor }

abstract class StoredArrayOfStringLists(fileName: String) {

  val dataSet = new ArrayBuffer[List[String]]

  val fullFileName = core.Environment.getConfigFilePath(fileName)

  val fileEncoding = "UTF-8"

  var initialized = false

  var minimumFieldCount = 0
  /** This field (if nonzero) is used for extending lists when reading from file.
    * Reassign if necessary to ensure lower bound on number of fields.
    * The reason is that trailing tabs are not stored to the file.
    */

  def fileExists: Boolean = FileMethods.IsFile(fullFileName)

  def loadFromFile() {

    def addLine(line: String) {
      val r = line.trim.split('\t').toList
      if (r.length > 1) dataSet += r.padTo(minimumFieldCount, "")
    }

    try {
      val src = Source fromFile (fullFileName, fileEncoding)
      src.getLines.foreach(line => addLine(line))
      src.close()
    } catch {
      case e: Exception => {
        editor.DialogBox.stackTrace("Could not read '" + fileName + "': " + e.getMessage, e)
      }
    }
  }

  def getKeyLength(stringList: List[String]): Int

  def getIndexOf(stringList: List[String]): Int = {
    val keyLength = getKeyLength(stringList)
    val lookupKey = stringList.slice(0, keyLength)
    dataSet.indexWhere(_.startsWith(lookupKey))
  }

  def update(stringList: List[String]) {
    val index = getIndexOf(stringList)
    if (index < 0) {
      dataSet += stringList.padTo(minimumFieldCount, "")
    } else {
      dataSet(index) = dataSet(index).patch(0, stringList, stringList.length)
    }
  }

  def updateFrom(keyList: List[String], offset: Int, stringList: List[String]) {
    val index = getIndexOf(keyList)
    if (index >= 0) {
      dataSet(index) = dataSet(index).
        patch(offset, stringList, stringList.length)
    }
  }

  def remove(key: List[String]) {
    val index = getIndexOf(key)
    if (index >= 0) dataSet.remove(index)
  }

  def saveToFile() {

    def asString(stringList: List[String]): String =
      stringList.mkString("", "\t", "\n")

    try {
      val outputStream = new FileOutputStream(fullFileName)
      val outFile = new OutputStreamWriter(outputStream, fileEncoding)
      dataSet.foreach(stringList => outFile.write(asString(stringList)))
      outFile.close()
    } catch {
      case e: Exception => editor.DialogBox.stackTrace(
        "Could not write to \"" + fullFileName + "\": " + e.getMessage, e)
    }
  }
}
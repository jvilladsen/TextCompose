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

  /*
   * Data set that is kept in sync with the file - with Load and Store methods.
   */
  val dataSet = new ArrayBuffer[List[String]]

  val fullFileName = core.Environment.getConfigFilePath(fileName)

  val fileEncoding = "UTF-8"

  var initialized = false

  var minimumFieldCount = 0
  /* This field (if nonzero) is used for extending lists when reading from file.
   * Reassign if necessary to ensure lower bound on number of fields.
   * The reason is that trailing tabs are not stored to the file.
   * If the data set contains a list with trailing empty string,
   * then they do not get written to the file.
   */

  /*
   * Does typically not exist first time, so handle that case before loading.
   */
  def fileExists: Boolean = FileMethods.IsFile(fullFileName)

  def load() = {

    def ParseAndAddToMap(line: String) {
      var x = line.trim.split('\t').toList
      val length = x.length
      if (length > 1) dataSet += x.padTo(minimumFieldCount, "")
    }

    // Try to open (and read) the mapping file.
    try {
      var src = Source fromFile (fullFileName, fileEncoding)
      src.getLines.foreach(line => ParseAndAddToMap(line))
      true
    } catch {
      case e: Exception => {
        editor.DialogBox.stackTrace("Could not read '" + fileName + "': " + e.getMessage, e)
      }
    }
  }

  def getKeyLength(stringList: List[String]): Int

  def getIndexOf(stringList: List[String]): Int = {

    def SameKey(c1: List[String], c2: List[String], keyLength: Int): Boolean = {
      val length1 = c1.length
      val length2 = c2.length
      if (length1 < keyLength || length2 < keyLength) { return false }

      var sameValues = true
      var i = 0
      while (i < keyLength && sameValues) {
        sameValues = c1(i) == c2(i)
        i += 1
      }
      return sameValues
    }

    val keyLength = getKeyLength(stringList)
    var matchingIndex = -1
    var index = 0
    for (c <- dataSet) {
      if (SameKey(c, stringList, keyLength)) { matchingIndex = index }
      index += 1
    }
    return matchingIndex
  }

  def update(stringList: List[String]) {
    // Adds the stringList if it is not found.
    val matchingIndex = getIndexOf(stringList)
    if (matchingIndex > -1) {
      dataSet(matchingIndex) = dataSet(matchingIndex).patch(0, stringList, stringList.length)
    } else {
      dataSet += stringList.padTo(minimumFieldCount, "")
    }
  }

  def updateFrom(keyList: List[String], offset: Int, stringList: List[String]) {
    // Patches stringList into data set
    val matchingIndex = getIndexOf(keyList)
    if (matchingIndex > -1) {
      dataSet(matchingIndex) = dataSet(matchingIndex).patch(offset, stringList, stringList.length)
    }
  }

  def remove(stringList: List[String]) {
    val matchingIndex = getIndexOf(stringList)
    if (matchingIndex > -1) {
      dataSet.remove(matchingIndex)
    }
  }

  def clear() { dataSet.clear() }

  def store() {

    def asString(stringList: List[String]): String = stringList.mkString("", "\t", "\n")

    try {
      val outputStream = new FileOutputStream(fullFileName)
      val outFile = new OutputStreamWriter(outputStream, fileEncoding)
      dataSet.foreach(stringList => outFile.write(asString(stringList)))
      outFile.close
    } catch {
      case e: Exception => editor.DialogBox.stackTrace("Could not write to \"" + fullFileName + "\": " + e.getMessage, e)
    }
  }
}
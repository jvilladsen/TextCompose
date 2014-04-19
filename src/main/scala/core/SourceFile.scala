/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.core

import scala.io.Source

class SourceFile(
  fileName: String,
  encoding: String,
  processingUnit: ProcessingUnit,
  root: Boolean) {

  var sourceHandle = Source fromFile (fileName, encoding)
  var sourceIterator = sourceHandle.getLines // iterator on source
  var lineContent = ""
  var lineNumber = 0L

  processingUnit.addFile(fileName, root)

  def readLine: Boolean = {
    val moreLines = sourceIterator.hasNext
    if (moreLines) {
      lineNumber += 1
      try {
        lineContent = sourceIterator.next // Since we don't trim here, we must take care of newline char in parser
        processingUnit.update(lineContent)
      } catch {
        case e: Exception => textcompose.editor.DialogBox.stackTrace(
          "\nError reading line " + lineNumber.toString + " from " + fileName + ". Wrong encoding?", e)
      }
    } else {
      processingUnit.popElement()
    }
    moreLines
  }
}
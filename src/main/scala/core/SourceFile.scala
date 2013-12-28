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
        case e: Exception => writesetter.editor.DialogBox.stackTrace(
          "\nError reading line " + lineNumber.toString + " from " + fileName + ". Wrong encoding?", e)
      }
    } else {
      processingUnit.popElement()
    }
    moreLines
  }
}
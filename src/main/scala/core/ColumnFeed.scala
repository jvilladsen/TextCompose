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

import scala.collection.mutable.Stack
import com.itextpdf.text._
import com.itextpdf.text.pdf._

class ColumnFeed(
  iTextDoc: Document,
  directContent: PdfContentByte,
  stateStack: Stack[State]) extends ColumnText(directContent) {

  private var left = 0f
  private var bottom = 0f
  private var right = 0f
  private var top = 0f

  private var columns = 0
  private var gutter = 0f

  private var columnCount = 1
  private var columnIsEmpty = true

  private def apply() {
    setSimpleColumn(left, bottom, right, top)
  }

  private def update() {
    bottom = stateStack.top.marginBottom
    top = iTextDoc.getPageSize.getHeight - stateStack.top.marginTop

    columns = stateStack.top.numberOfColumns
    gutter = stateStack.top.columnGutterSize

    val leftMargin = stateStack.top.marginLeft
    val effectiveWidth = iTextDoc.getPageSize.getWidth - (stateStack.top.marginRight + stateStack.top.marginLeft) - (columns - 1) * gutter
    val columnWidth = effectiveWidth / columns

    left = leftMargin + (columnCount - 1) * (columnWidth + gutter)
    if (columnCount == columns) {
      right = iTextDoc.getPageSize.getWidth - stateStack.top.marginRight
    } else {
      right = leftMargin + columnCount * columnWidth + (columnCount - 1) * gutter
    }
  }

  def goToNext() {
    columnCount = if (columnCount == columns) 1 else columnCount + 1
    update()
    apply()
  }

  def goToFirst() {
    columnCount = 1
    update()
    apply()
  }

  def isLastColumnOnPage = columnCount == columns

  def getCoordinates = (left, bottom, right, top)

  def newColumn(newPage: Boolean) {
    if (newPage || isLastColumnOnPage) {
      iTextDoc.newPage()
      goToFirst()
    } else {
      goToNext()
    }
    setYLine(iTextDoc.getPageSize.getHeight - stateStack.top.marginTop)
    columnIsEmpty = true
  }

  def addColumnFeed() {
    val before = stateStack.top.spaceBeforeParagraph.getValueOrPercentageOfNumber(stateStack.top.fontSize)
    val after = stateStack.top.spaceAfterParagraph.getValueOrPercentageOfNumber(stateStack.top.fontSize)

    if (!columnIsEmpty) setYLine(getYLine - before)
    var goOnWriting = true
    var emptyRounds = 0
    while (goOnWriting) {
      val priorYLine = getYLine

      val status = go()

      // For tables getLinesWritten is zero (and so is getRowsDrawn !) so hacked using getYLine.
      if (getLinesWritten == 0 && getYLine == priorYLine) {
        if (columnIsEmpty) {
          /* To avoid the infinite loop adding one page after the next because the content
					 * in the buffer is too large to fit on the page - typically an image.
					 */
          if (emptyRounds > 10) {
            throw new TagError("The page size (excluding margin) it too small for adding content to the document.")
          } else {
            emptyRounds += 1
          }
        }
      } else {
        emptyRounds = 0
        columnIsEmpty = false
      }
      goOnWriting = ColumnText.hasMoreText(status)
      if (goOnWriting) newColumn(false)
    }
    setYLine(getYLine - after)
  }
}
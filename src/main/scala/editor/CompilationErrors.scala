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

import scala.collection.mutable.ArrayBuffer
import textcompose.core

class CompilationErrors {

  private var numberOfErrors = 0
  private val errorCols = new TableColumns
  errorCols.add(0, "No.", "Int", "Int", 30, 50, false)
  errorCols.add(1, "Error", "String", "String", 500, 900, true)
  errorCols.add(2, "Location", "String", "String", 500, 700, true)

  private val sortingFields = new ColumnOrdering(errorCols)
  sortingFields.add(0, false) // ascending by number

  private var compilerErrors = new ArrayBuffer[List[String]]

  private var tableContent = new TableContent(
    compilerErrors,
    errorCols,
    sortingFields,
    16, // font size
    Colors.editorBackground,
    Colors.editorForeground,
    Colors.overviewGrid,
    Colors.editorBackground,
    Colors.standard)

  def update() {
    compilerErrors = new ArrayBuffer[List[String]]
    var counter = 0
    for ((message, location) <- core.CompilationMetaData.getErrorMessages) {
      counter += 1
      compilerErrors += List(counter.toString, message, location)
    }
    numberOfErrors = counter
    tableContent.newDataSet(compilerErrors)
    tableContent.updateTableData(true)
  }

  tableContent.updateTableData(true)

  def getPane = tableContent.tableInScrollPane

  def updateColors() { tableContent.updateColors() }

  def getNumberOfErrors = numberOfErrors
}
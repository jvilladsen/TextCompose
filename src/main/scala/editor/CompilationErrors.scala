/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
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
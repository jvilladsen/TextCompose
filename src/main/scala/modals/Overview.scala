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

package textcompose.modals

import scala.swing._
import javax.swing.JTable
import scala.collection.mutable.Set
import event._
import event.Key._
import textcompose.{ editor, storage }
import textcompose.editor.Colors

class Overview extends Dialog {

  /* Table:
   * - Consider which factors should determine the size of the window.
   * - Time stamps: consider not showing date if date is current date
   * 		and consider not showing year if current year etc.
   * - Respect users changes to column width and column order when re-sorting table.
   */
  private val cols = new editor.TableColumns
  cols.add(1, "Name", "String", "String", 260, 400, false)
  cols.add(2, "Created", "Long", "Time", 150, 170, false)
  cols.add(3, "Changed", "Long", "Time", 150, 170, false)
  cols.add(4, "Encoding", "String", "String", 90, 110, false)
  cols.add(5, "Language", "String", "String", 90, 110, false)
  cols.add(6, "Pages", "Int", "Int", 60, 70, false)
  cols.add(7, "Errors", "Int", "Int", 60, 70, false)
  cols.add(8, "Built", "Long", "Time", 150, 170, false)
  cols.add(9, "Build time", "Int", "Int", 60, 80, false)
  cols.add(0, "Full name", "String", "String", 300, 500, false)

  private val sortingFields = new editor.ColumnOrdering(cols)
  sortingFields.add(2, true) // initially sort descending by field 'Changed Date'

  val tableContent = new editor.TableContent(
    storage.SourcesMetaData.dataSet,
    cols,
    sortingFields,
    16, // font size
    Colors.overviewBackground,
    Colors.overviewForeground,
    Colors.overviewGrid,
    Colors.selectionBackground,
    Colors.selectionForeground)

  private var fileSelection: Set[String] = null
  private var confirmed = false

  tableContent.updateTableData(true)

  tableContent.enableColumnSorting()

  private val openAction = new Action("Open") {
    enabled = true
    def apply() {
      fileSelection = tableContent.getSelection(0)
      val sizeOfSelection = fileSelection.size
      if (sizeOfSelection > 7) {
        confirmed = editor.DialogBox.warning("Open " + sizeOfSelection.toString + " files?")
      } else {
        confirmed = true
      }
      if (confirmed) {
        close
        dispose
      }
    }
  }
  private val cancelAction = new Action("Close") {
    enabled = true
    def apply() {
      close
      dispose
    }
  }
  tableContent.setActionForEnterAndEscape(openAction, cancelAction)

  private val applicationWindowSize = editor.Application.top.size

  contents = tableContent.tableInScrollPane

  modal = true
  minimumSize = applicationWindowSize
  maximumSize = applicationWindowSize
  centerOnScreen
  open

  def getSelection = if (confirmed) fileSelection.toList else List()
}
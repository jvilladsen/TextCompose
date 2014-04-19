/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.modals

import scala.swing._
import javax.swing.JTable
import scala.collection.mutable.Set
import event._
import event.Key._
import textcompose.{ editor, storage }
import textcompose.editor.Colors

class Fonts(viewIssues: Boolean) extends Dialog {

  /* Table:
   * - Consider which factors should determine the size of the window.
   * - Time stamps: consider not showing date if date is current date
   * 		and consider not showing year if current year etc.
   * - Respect users changes to column width and column order when re-sorting table.
   */
  /* FORMAT:
   * font file name -- is the primary key
   * can be installed (true/false)
   * can be embedded (true/false)
   * error message in case one of the above is false (the first false)
   * name of font
   * title of font
   * version
   * copyright
   * familyName
   * subFamilyName
   * uniqueId
   * trademark
   * manufacturer
   * designer
   * description
   * vendorURL
   * designerURL
   * license
   * licenseURL
   * sampleText
   * encodings
   */

  private val cols = new editor.TableColumns
  cols.add(5, "Title", "String", "String", 200, 250, false)
  if (viewIssues) {
    cols.add(3, "Issue", "String", "String", 650, 850, false)
  } else {
    cols.add(6, "Version", "String", "String", 70, 120, false)
    cols.add(8, "Family", "String", "String", 100, 120, false)
    cols.add(9, "Form", "String", "String", 50, 70, false)
    cols.add(13, "Designer", "String", "String", 140, 170, false)
    cols.add(12, "Manufacturer", "String", "String", 140, 170, false)
    cols.add(10, "ID", "String", "String", 100, 200, false)
    cols.add(17, "Licence", "String", "String", 300, 500, false)
    cols.add(7, "Copyright", "String", "String", 100, 300, false)
    cols.add(11, "Trademark", "String", "String", 150, 250, false)
    cols.add(15, "Vendor URL", "String", "String", 130, 300, false)
    cols.add(16, "Designer URL", "String", "String", 130, 300, false)
    cols.add(14, "Description", "String", "String", 300, 500, false)
    cols.add(3, "Embedding", "String", "String", 300, 500, false)
  }

  private val sortingFields = new editor.ColumnOrdering(cols)
  sortingFields.add(0, false) // initially sort descending by field ???

  val installStatus = if (viewIssues) "false" else "true"
  val fontData = storage.StoredFontAnalysis.dataSet.filter(p => p(1) == installStatus)

  val tableContent = new editor.TableContent(
    fontData,
    cols,
    sortingFields,
    16, // font size
    Colors.overviewBackground,
    Colors.overviewForeground,
    Colors.overviewGrid,
    Colors.selectionBackground,
    Colors.selectionForeground)

  private var fontSelection: Set[String] = null
  private var confirmed = false

  tableContent.updateTableData(true)

  tableContent.enableColumnSorting()

  private val openAction = new Action("Open") {
    enabled = true
    def apply() {
      fontSelection = tableContent.getSelection(5)
      val sizeOfSelection = fontSelection.size
      if (sizeOfSelection > 7) {
        confirmed = editor.DialogBox.warning("Open " + sizeOfSelection.toString + " fonts?")
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

  def getSelection = if (confirmed) fontSelection.toList else List()
}
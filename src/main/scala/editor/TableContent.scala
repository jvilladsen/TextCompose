/** This file is part of TextCompose, a program for producing PDF from text files.
  * Copyright 2014  Jesper S Villadsen <jeschvi@gmail.com>
  * License: GNU Affero General Public License version 3 or later.
  * For full license text see LICENSE.txt or <http://www.gnu.org/licenses/>.
  */

package textcompose.editor

import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.swing._
import javax.swing.table.AbstractTableModel
import scala.swing.TextArea
import javax.swing.JTable
import event._
import event.Key._
import java.awt.Graphics
import java.awt.event.{ MouseEvent, MouseAdapter }
import textcompose.storage

class TableContent(
  d: ArrayBuffer[List[String]], // FIXME: should not be parameter?
  cols: TableColumns,
  sortingFields: ColumnOrdering,
  fontSize: Int,
  back: java.awt.Color,
  fore: java.awt.Color,
  grid: java.awt.Color,
  selectionBack: java.awt.Color,
  selectionFore: java.awt.Color) {

  private var dataSet = d
  private var numberOfRows = dataSet.length
  private var sortedTableData: List[List[String]] = null

  class LocalTableModel(var rowData: Array[Array[Any]], val columnNames: Seq[String]) extends AbstractTableModel {
    override def getColumnName(column: Int) = columnNames(column).toString
    def getRowCount() = rowData.length
    def getColumnCount() = columnNames.length
    def getValueAt(row: Int, col: Int): AnyRef = rowData(row)(col).asInstanceOf[AnyRef]
    override def isCellEditable(row: Int, column: Int) = false
    override def setValueAt(value: Any, row: Int, col: Int) { rowData(row)(col) = value }
  }

  def newDataSet(d: ArrayBuffer[List[String]]) {
    dataSet = d
    numberOfRows = dataSet.length
  }

  private val table = if (cols.hasAnyWordWrap) {
    new Table(cols.getNumberOfColumns, numberOfRows) {
      font = storage.GUIFonts.getStandardFont(fontSize)
      rowHeight = fontSize + 8
      background = back
      foreground = fore
      gridColor = grid
      selectionBackground = selectionBack
      selectionForeground = selectionFore
      border = Swing.EmptyBorder(20, 20, 20, 20)
      peer.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

      override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
        val v = model.getValueAt(
          peer.convertRowIndexToModel(row),
          peer.convertColumnIndexToModel(col)).toString

        val y = new TextArea {
          text = v
          font = storage.GUIFonts.getStandardFont(fontSize)
          background = if (sel) selectionBack else back
          foreground = if (sel) selectionFore else fore
          border = Swing.EmptyBorder(1, 5, 0, 5)
          if (wrapCols.contains(col)) {
            lineWrap = true
            wordWrap = true
          }
        }
        y
      }
    }
  } else {
    new Table(cols.getNumberOfColumns, numberOfRows) {
      font = storage.GUIFonts.getStandardFont(fontSize)
      rowHeight = fontSize + 8
      background = back
      foreground = fore
      gridColor = grid
      selectionBackground = selectionBack
      selectionForeground = selectionFore
      border = Swing.EmptyBorder(20, 20, 20, 20)
      peer.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
    }
  }

  if (cols.hasAnyWordWrap) {
    table.listenTo(table)
    table.reactions += {
      case r: TableResized => {
        updateRowHeights()
      }
    }
  }

  val tableInScrollPane = new ScrollPane {
    verticalScrollBar.unitIncrement = 9
    horizontalScrollBar.unitIncrement = 9
    border = Swing.EmptyBorder(0, 0, 0, 0)
    background = back
    foreground = fore
    contents = table
  }

  private val wordWidthOfColumn = new HashMap[(Int, Int), Int]
  private val wrapCols = cols.getWordWrapColumns
  private val fontMetrics = table.peer.getFontMetrics(table.font)
  private def calculateMaxContentWidth() {
    var r = 0
    for (row <- dataSet) {
      for (c <- wrapCols) {
        val stringWidth = fontMetrics.stringWidth(row(c))
        wordWidthOfColumn((r, c)) = stringWidth
      }
      r += 1
    }
  }
  private def updateRowHeights() {
    val columnWidths = new HashMap[Int, Int]
    for (c <- wrapCols) {
      columnWidths(c) = table.peer.getColumnModel().getColumn(c).getWidth
    }
    var r = 0
    while (r < numberOfRows) {
      var rowHeight = 0
      for (c <- wrapCols) {
        val cellHeight = (wordWidthOfColumn((r, c)) / (columnWidths(c) - 10) + 1) * (fontSize + 6) + 2
        rowHeight = Math.max(rowHeight, cellHeight)
      }
      table.peer.setRowHeight(r, rowHeight)
      r += 1
    }
  }

  def updateTableData(calculateWordWidth: Boolean) {
    sortedTableData = dataSet.toList.sortWith((a, b) => sortingFields.lessThan(a, b))

    def getData(row: Int, column: Int) = {
      val storageColumn = cols.getSourceColumn(column)
      val data = sortedTableData(row)(storageColumn)
      cols.formated(data, column)
    }
    val numberOfColumns = cols.getNumberOfColumns
    val rowData = Array.tabulate[Any](numberOfRows, numberOfColumns) { getData(_, _) }
    val colNames = sortingFields.getDecoratedColumnNames()

    table.model = new LocalTableModel(rowData, colNames)

    var index = 0
    while (index < numberOfColumns) {
      val col = table.peer.getColumnModel().getColumn(index)
      val width = cols.getWidth(index)
      col.setMinWidth(width._1)
      col.setPreferredWidth(width._2)
      index += 1
    }
    if (calculateWordWidth && cols.hasAnyWordWrap) {
      calculateMaxContentWidth()
      updateRowHeights()
    }
  }

  def getNumberOfRows = numberOfRows

  def enableColumnSorting() {
    val header = table.peer.getTableHeader()
    header.addMouseListener(new MouseAdapter() {
      override def mouseClicked(e: MouseEvent) {
        val col = header.columnAtPoint(e.getPoint())
        sortingFields.add(col, true)
        updateTableData(false)
      }
    })
  }

  def setActionForEnterAndEscape(enterAction: Action, escapeAction: Action) {
    table.listenTo(table.keys)
    table.reactions += {
      case KeyPressed(`table`, Enter, _, _)  => { enterAction.apply() }
      case KeyPressed(`table`, Escape, _, _) => { escapeAction.apply() }
    }
  }
  def getSelection(c: Int) = table.selection.rows.map(sortedTableData(_)(c))

  def updateColors() {
    table.background = back
    table.foreground = fore
    tableInScrollPane.background = back
    tableInScrollPane.foreground = fore
    updateTableData(false)
  }
}